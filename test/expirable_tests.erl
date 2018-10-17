%%%-------------------------------------------------------------------
%%% @author James
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 15. May 2016 12:45 PM
%%%-------------------------------------------------------------------
-module(expirable_tests).
-author("mb-spare").

-include_lib("eunit/include/eunit.hrl").
-include("../include/test_values.hrl").

initiate_test() ->
  Task = fun(_,_,Data) -> {done,Data} end,
  expirable_sup:start_link(),
  {ok,Pid,_Code} = expirable_sup:start_expirable(#expirable_state{task = Task,data = do_this,timeout =3000 }),
  ?assertNotEqual(undefined,erlang:process_info(Pid)).

do_test()->
  Task = fun(_,_,Data) -> {done,Data} end,
  {ok,Pid,Name} = expirable_sup:start_expirable(#expirable_state{task = Task,data = do_this,timeout =3000 }),
  ?assertNotEqual(undefined,erlang:process_info(Pid)),
  Return = expirable_fsm:do_task(Name,data),
  ?assertEqual({done,do_this},Return),
  ?assertEqual(undefined,erlang:process_info(Pid)).

initiate_already_dead_test()->
  Task = fun(_,_,Data) -> {done,Data} end,
  {ok,Pid,Name} = expirable_sup:start_expirable(#expirable_state{task = Task,data = do_this,timeout =3000 }),
  ?assertNotEqual(undefined,erlang:process_info(Pid)),
  Return = expirable_fsm:do_task(Name,data),
  ?assertEqual({done,do_this},Return),
  Error = expirable_fsm:do_task(Name,data),
  ?assertEqual({error,not_present},Error).

initiate_already_initiated_test()->
  Task = fun(_,_,Data) -> {done,Data} end,
  {ok,_Pid,_Name}= expirable_sup:start_expirable(#expirable_state{task = Task,data = do_this,timeout =3000 }),
  {ok,_Pid2,_Name2} = expirable_sup:start_expirable(#expirable_state{task = Task,data = do_this,timeout =3000 }).

expire_test()->
  Task = fun(_,_,Data) -> {done,Data} end,
  {ok,Pid,_Name}= expirable_sup:start_expirable(#expirable_state{task = Task,data = do_this,timeout =3000 }),
  ?assertNotEqual(undefined,erlang:process_info(Pid)),
  receive
    nothing -> nothing
  after 4000 ->
    ?assertEqual(undefined,erlang:process_info(Pid))
  end.

get_state_test()->
  Task = fun(_,_,Data) -> {done,Data} end,
  DataPassed = do_this,
  Timeout = 3000,
  {ok,Pid,Name}= expirable_sup:start_expirable(#expirable_state{task = Task,data = DataPassed,timeout =Timeout }),
  ?assertNotEqual(undefined,erlang:process_info(Pid)),
  receive
    nothing -> nothing
  after 2000 ->
    {ok,{StateName, State}} = expirable_fsm:get_state(Name),
    ?assertEqual(initiated,StateName),
    ?assert(is_record(State,expirable_state)),
    ?assertEqual(DataPassed,State#expirable_state.data),
    ?assertEqual(Timeout,State#expirable_state.timeout),
    ?assertEqual(Task,State#expirable_state.task)
  end.
get_state_not_present_test()->
  Task = fun(_,_,Data) -> {done,Data} end,
  DataPassed = do_this,
  Timeout = 2000,
  {ok,Pid,Name}= expirable_sup:start_expirable(#expirable_state{task = Task,data = DataPassed,timeout =Timeout }),
  ?assertNotEqual(undefined,erlang:process_info(Pid)),
  receive
    nothing -> nothing
  after 3000 ->
    Return = expirable_fsm:get_state(Name),
    ?assertEqual({error,not_present},Return)
  end.

expire_do_test()->
  expirable_sup:start_link(),
  Task = fun(_,_,Data) -> {done,Data} end,
  {ok,Pid,Name} = expirable_sup:start_expirable(#expirable_state{task = Task,data = do_this,timeout =3000 }),
  ?assertNotEqual(undefined,erlang:process_info(Pid)),
  receive
  after 4000 ->
    Return = expirable_fsm:do_task(Name,data),
    ?assertEqual({error,not_present},Return)
  end.


expire_while_doing_test()->
  Task = fun(_,_,Data) ->
    receive
      nothing -> nothing
    after 3000 ->
      ?debugMsg(doing),
      {done,Data}
    end
  end,
  Data = do_this,
  {ok,Pid,Name} = expirable_sup:start_expirable(#expirable_state{task = Task,data =Data ,timeout =2000 }),
  ?assertNotEqual(undefined,erlang:process_info(Pid)),
  Return = expirable_fsm:do_task(Name,data),
  ?assertEqual({done,do_this},Return).



do_again_already_done_test()->
  expirable_sup:start_link(),
  Task = fun(_,_,Data) ->
    receive
      nothing -> nothing
    after 3000 ->
      ?debugMsg("task================================"),
      {done,Data}
    end
  end,
  Data = do_this,
  {ok,Pid,Name}= expirable_sup:start_expirable(#expirable_state{task = Task,data =Data ,timeout =2000 }),
  ?assertNotEqual(undefined,erlang:process_info(Pid)),
  Return1 = expirable_fsm:do_task(Name,data),
  ?assertEqual({done,do_this},Return1),
  ?debugMsg(here1),
  Return2 = expirable_fsm:do_task(Name,data),
  ?debugMsg(here2),
  ?assertEqual({error,not_present},Return2).


do_again_while_doing_test()->
  expirable_sup:start_link(),
  Task = fun(_,_,Data) ->
    receive
      nothing -> nothing
    after 3000 ->
      ?debugMsg("expirable task================================"),
      {done,Data}
    end
  end,
  Data = do_this,
  {ok,Pid,Name} = expirable_sup:start_expirable(#expirable_state{task = Task,data =Data ,timeout =1000 }),
  ?assertNotEqual(undefined,erlang:process_info(Pid)),
   spawn(fun(TheName)->
     receive
     after 2000 ->
       ?debugMsg("async task================================"),
       Return2 =  expirable_fsm:do_task(TheName,data),
       ?assertEqual({error,already_doing},Return2)
     end
   end),
  Return1 = expirable_fsm:do_task(Name,data),
  ?assertEqual({done,do_this},Return1),
  Return = expirable_fsm:do_task(Name,data),
  ?assertEqual({error,not_present},Return)
.

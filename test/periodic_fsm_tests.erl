%%%-------------------------------------------------------------------
%%% @author James
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 18. Oct 2016 3:32 PM
%%%-------------------------------------------------------------------
-module(periodic_fsm_tests).
-author("james").

-include_lib("eunit/include/eunit.hrl").


-include_lib("eunit/include/eunit.hrl").
-include("../include/test_values.hrl").

initiate_test() ->
  Task = fun(OldState = #periodic_state{data = Accumulator}) ->
    OldState#periodic_state{data = Accumulator + 1}
         end,
  periodic_sup:start_link(),
  {ok, Pid, _Code} = periodic_sup:start_periodic(#periodic_state{type = custom, task = Task, data = 0, interval = 3000, max = 3,start_after = 1000}),
  ?assertNotEqual(undefined, erlang:process_info(Pid)).

do_test() ->
  Task = fun(OldState = #periodic_state{data = Accumulator}) ->
    OldState#periodic_state{data = Accumulator + 1}
         end,
  {ok, Pid, Name} = periodic_sup:start_periodic(#periodic_state{type = custom, task = Task, data = 0, interval = 1000, max = 3,start_after = 1000}),
  ?assertNotEqual(undefined, erlang:process_info(Pid)),
  receive
  after 1100 ->

    {ok, {started, PeriodicState}} = periodic_fsm:get_state(Name),
    ?debugFmt("~w ~n", [PeriodicState]),
    ?assertEqual(1, PeriodicState#periodic_state.number_of_calls),
    ?assertEqual(1, PeriodicState#periodic_state.data)
  end,
  receive
  after 500 ->
    {ok, {started, PeriodicState1}} = periodic_fsm:get_state(Name),
    ?assertEqual(1, PeriodicState1#periodic_state.number_of_calls),
    ?assertEqual(1, PeriodicState1#periodic_state.data)
  end,
  receive
  after 700 ->
    {ok, {started, PeriodicState2}} = periodic_fsm:get_state(Name),
    ?assertEqual(2, PeriodicState2#periodic_state.number_of_calls),
    ?assertEqual(2, PeriodicState2#periodic_state.data)
  end,
  receive
  after 500 ->
    {ok, {started, PeriodicState3}} = periodic_fsm:get_state(Name),
    ?assertEqual(2, PeriodicState3#periodic_state.number_of_calls),
    ?assertEqual(2, PeriodicState3#periodic_state.data)
  end,
  receive
  after 700 ->
    {ok, {started, PeriodicState4}} = periodic_fsm:get_state(Name),
    ?assertEqual(3, PeriodicState4#periodic_state.data)
  end,
  receive
  after 700 ->
    ?assertEqual(undefined, erlang:process_info(Pid))
  end
.


stop_test() ->
  Task = fun(OldState = #periodic_state{data = Accumulator}) ->
    Stop = if
             Accumulator =:= 1 -> true;
             true -> false
           end,
    OldState#periodic_state{data = Accumulator + 1, stop = Stop}
         end,
  {ok, Pid, Name} = periodic_sup:start_periodic(#periodic_state{type = custom, task = Task, data = 0, interval = 1000, max = 3,start_after = 1000}),
  ?assertNotEqual(undefined, erlang:process_info(Pid)),
  receive
  after 1100 ->
    {ok, {started, PeriodicState}} = periodic_fsm:get_state(Name),
    ?assertEqual(1, PeriodicState#periodic_state.number_of_calls),
    ?assertEqual(1, PeriodicState#periodic_state.data)
  end,
  receive
  after 500 ->
    {ok, {started, PeriodicState1}} = periodic_fsm:get_state(Name),
    ?assertEqual(1, PeriodicState1#periodic_state.number_of_calls),
    ?assertEqual(1, PeriodicState1#periodic_state.data)
  end,
  receive
  after 2000 ->
    ?assertEqual({error, not_present}, periodic_fsm:get_state(Name)),
    ?assertEqual(undefined, erlang:process_info(Pid))
  end.
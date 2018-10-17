%%%-------------------------------------------------------------------
%%% @author james
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%% <p>upon initiation the fsm enters an initiated state</p>
%% <p>Initiation also involves starting the timer</p>
%% <p>when one wants to do the task one calls the do_task function which
%% first runs a sync event to put the fsm in the doing state,
%% then it runs another sync event that will start the task and kill the process when its done</p>
%% <p>If the fsm doesnt receive the do task event before time out it will expire</p>
%% <p>If the caller wants to perfom an asynchronous task then they can use the do_task to send
%% a message to an async_runner process to start and it will return something like 'ok', and the expirable fsm will die after that (starting the async task)
%% but the async task will continue to run</p>
%% <p><b>States:</b> initiated, doing</p>
%% <p><b>Events:</b> get_ready, do_task, expire</p>
%%% @end
%%% Created : 15. May 2016 1:21 PM
%%%-------------------------------------------------------------------
-module(expirable_fsm).
-author("james").

-behaviour(gen_fsm).

-include("../include/periodic_app_common.hrl").

%% API
-export([start_link/0,start_link_sup/1,do_task/3,do_task/2,get_state/1]).

%% gen_fsm callbacks
-export([init/1,
  doing/2,
  doing/3,

  initiated/2,
  initiated/3,

  handle_event/3,
  handle_sync_event/4,
  handle_info/3,
  terminate/3,
  code_change/4]).

-define(SERVER, ?MODULE).


%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Creates a gen_fsm process which calls Module:init/1 to
%% initialize. To ensure a synchronized start-up procedure, this
%% function does not return until Module:init/1 has returned.
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link() -> {ok, pid()} | ignore | {error, Reason :: term()}).
start_link() ->
  gen_fsm:start_link({local, ?SERVER}, ?MODULE, [], []).

%%--------------------------------------------------------------------
%% @doc
%% Creates a gen_fsm process which calls Module:init/1 to
%% initialize. To ensure a synchronized start-up procedure, this
%% function does not return until Module:init/1 has returned.
%% this one is for the supervisor
%% @todo test this
%%
%% @end
%%--------------------------------------------------------------------
start_link_sup(ParameterAsState = #expirable_state{name = RegName}) ->
  gen_fsm:start_link({global, RegName}, ?MODULE, ParameterAsState, []).


%% @doc this will do the expirable task
do_task(RegName,Data) ->
  do_task(RegName,Data,infinity).

-spec(do_task(RegName :: atom() ,SecondaryData :: term() , Timeout :: timeout()) -> ok|{error, Message :: term()} ).
do_task(RegName,SecondaryData,Timeout) ->

  case
      try
      gen_fsm:sync_send_event({global, RegName}, get_ready,50)
        catch
          exit:{noproc,_} -> {error, not_present};
          exit:{timeout,_} -> {error, already_doing}
      end
  of
    ok -> gen_fsm:sync_send_event({global, RegName}, {do_task,SecondaryData},Timeout);
    {error, Reply} -> {error, Reply}
 end
.

%% @doc get the state of the expirable fsm
-spec(get_state(RegName :: atom()) -> {StateName :: atom() ,State :: #expirable_state{}} |{error, not_present} ).
get_state(RegName) ->
  try
    gen_fsm:sync_send_all_state_event({global, RegName}, get_state)
  catch
    exit:{noproc,_} -> {error, not_present}
  end
  .



%%%===================================================================
%%% gen_fsm callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm is started using gen_fsm:start/[3,4] or
%% gen_fsm:start_link/[3,4], this function is called by the new
%% process to initialize.
%%
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
  {ok, StateName :: atom(), StateData :: #expirable_state{}} |
  {ok, StateName :: atom(), StateData :: #expirable_state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).
init(ParameterAsState = #expirable_state{timeout = Time}) ->
  process_flag(trap_exit, true),
 Ref = gen_fsm:send_event_after(Time,expire),
  {ok, initiated, ParameterAsState#expirable_state{timeout_ref = Ref}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% <p>when the function do_task is run, the get_ready sync event is sent and the fsm enters the doing state
%% this is followed by the do_task sync event which will perfom the task and then kill the process
%% if the fsm receives an expire event while it is still in state doing
%% it will ignore this message
%% if the fms receives an expire event while in state initiated it will expire</p>
%% @end
%%--------------------------------------------------------------------
-spec(doing(Event :: term(), State :: atom()) ->
  {next_state, NextStateName :: atom(), NextState :: #expirable_state{}} |
  {next_state, NextStateName :: atom(), NextState :: #expirable_state{},
    timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #expirable_state{}}).
doing(expire,State)->
{next_state, doing,State}.

-spec(initiated(Event :: term(), State :: atom()) ->
  {next_state, NextStateName :: atom(), NextState :: #expirable_state{}} |
  {next_state, NextStateName :: atom(), NextState :: #expirable_state{},
    timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #expirable_state{}}).
initiated(expire,State)->
  {stop,normal,State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% <p>when the function do_task is run, the get_ready sync event is sent and the fsm enters the doing state
%% this is followed by the do_task sync event which will perfom the task and then kill the process
%% if the fsm receives an expire event while it is still in state doing
%% it will ignore this message
%% if the fms receives an expire event while in state initiated it will expire</p>
%%
%% @end
%%--------------------------------------------------------------------
-spec(initiated(Event :: term(), From :: {pid(), term()},
    State :: #expirable_state{}) ->
  {next_state, NextStateName :: atom(), NextState :: #expirable_state{}} |
  {next_state, NextStateName :: atom(), NextState :: #expirable_state{},
    timeout() | hibernate} |
  {reply, Reply, NextStateName :: atom(), NextState :: #expirable_state{}} |
  {reply, Reply, NextStateName :: atom(), NextState :: #expirable_state{},
    timeout() | hibernate} |
  {stop, Reason :: normal | term(), NewState :: #expirable_state{}} |
  {stop, Reason :: normal | term(), Reply :: term(),
    NewState :: #expirable_state{}}).
initiated(get_ready, _From, State) ->
  Reply = ok,
  {reply, Reply, doing, State}.



-spec(doing(Event :: term(), From :: {pid(), term()},
    State :: #expirable_state{}) ->
  {next_state, NextStateName :: atom(), NextState :: #expirable_state{}} |
  {next_state, NextStateName :: atom(), NextState :: #expirable_state{},
    timeout() | hibernate} |
  {reply, Reply, NextStateName :: atom(), NextState :: #expirable_state{}} |
  {reply, Reply, NextStateName :: atom(), NextState :: #expirable_state{},
    timeout() | hibernate} |
  {stop, Reason :: normal | term(), NewState :: #expirable_state{}} |
  {stop, Reason :: normal | term(), Reply :: term(),
    NewState :: #expirable_state{}}).
doing(get_ready, _From, State) ->
  Reply = already_doing_task,
  {reply, Reply, doing, State};
%% @todo maybe enclose in try and catch otherwise it may never stop if there is an error
doing({do_task,SecondaryData},From,State = #expirable_state{task = Task,data = Data}) ->

  Reply = Task(From,SecondaryData,Data),
  {stop, normal, Reply,State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm receives an event sent using
%% gen_fsm:send_all_state_event/2, this function is called to handle
%% the event.
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_event(Event :: term(), StateName :: atom(),
    StateData :: #expirable_state{}) ->
  {next_state, NextStateName :: atom(), NewStateData :: #expirable_state{}} |
  {next_state, NextStateName :: atom(), NewStateData :: #expirable_state{},
    timeout() | hibernate} |
  {stop, Reason :: term(), NewStateData :: #expirable_state{}}).
handle_event(_Event, StateName, State) ->
  {next_state, StateName, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm receives an event sent using
%% gen_fsm:sync_send_all_state_event/[2,3], this function is called
%% to handle the event.
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_sync_event(Event :: term(), From :: {pid(), Tag :: term()},
    StateName :: atom(), StateData :: term()) ->
  {reply, Reply :: term(), NextStateName :: atom(), NewStateData :: term()} |
  {reply, Reply :: term(), NextStateName :: atom(), NewStateData :: term(),
    timeout() | hibernate} |
  {next_state, NextStateName :: atom(), NewStateData :: term()} |
  {next_state, NextStateName :: atom(), NewStateData :: term(),
    timeout() | hibernate} |
  {stop, Reason :: term(), Reply :: term(), NewStateData :: term()} |
  {stop, Reason :: term(), NewStateData :: term()}).
handle_sync_event(get_state, _From, StateName, State) ->
  Reply = {ok,{StateName, State}},
  {reply, Reply, StateName, State};
handle_sync_event(_Event, _From, StateName, State) ->
  Reply = ok,
  {reply, Reply, StateName, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_fsm when it receives any
%% message other than a synchronous or asynchronous event
%% (or a system message).
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_info(Info :: term(), StateName :: atom(),
    StateData :: term()) ->
  {next_state, NextStateName :: atom(), NewStateData :: term()} |
  {next_state, NextStateName :: atom(), NewStateData :: term(),
    timeout() | hibernate} |
  {stop, Reason :: normal | term(), NewStateData :: term()}).
handle_info(_Info, StateName, State) ->
  {next_state, StateName, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_fsm when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_fsm terminates with
%% Reason. The return value is ignored.
%%
%% @end
%%--------------------------------------------------------------------
-spec(terminate(Reason :: normal | shutdown | {shutdown, term()}
| term(), StateName :: atom(), StateData :: term()) -> term()).
terminate(_Reason, _StateName, _State) ->
  ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @end
%%--------------------------------------------------------------------
-spec(code_change(OldVsn :: term() | {down, term()}, StateName :: atom(),
    StateData :: #expirable_state{}, Extra :: term()) ->
  {ok, NextStateName :: atom(), NewStateData :: #expirable_state{}}).
code_change(_OldVsn, StateName, State, _Extra) ->
  {ok, StateName, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

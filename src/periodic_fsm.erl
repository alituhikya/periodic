%%%-------------------------------------------------------------------
%%% @author james
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%% this does a particular task periodically
%%% @end
%%% Created : 18. Oct 2016 3:31 PM
%%%-------------------------------------------------------------------
-module(periodic_fsm).
-author("james").

-behaviour(gen_fsm).

-include("../include/periodic_app_common.hrl").

%% API
-export([start_link/0, start_link_sup/1, get_state/1]).

%% gen_fsm callbacks
-export([init/1,
  started/2,
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
start_link_sup(ParameterAsState = #periodic_state{name = RegName}) ->
  gen_fsm:start_link({global, RegName}, ?MODULE, ParameterAsState, []).


%% @doc get the state of the periodic fsm
-spec(get_state(RegName :: atom()) -> {StateName :: atom(), State :: #periodic_state{}} |{error, not_present}).
get_state(RegName) ->
  try
    gen_fsm:sync_send_all_state_event({global, RegName}, get_state)
  catch
    exit:{noproc, _} -> {error, not_present}
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
  {ok, StateName :: atom(), StateData :: #periodic_state{}} |
  {ok, StateName :: atom(), StateData :: #periodic_state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).
init(ParameterAsState = #periodic_state{start_after = Time}) ->
  process_flag(trap_exit, true),
  Ref = gen_fsm:send_event_after(Time, do),
  {ok, started, ParameterAsState#periodic_state{interval_ref = Ref}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% <p>when the function do is run, the get_ready sync event is sent and the fsm enters the doing state
%% this is followed by the do_task sync event which will perfom the task and then kill the process
%% if the fsm receives an expire event while it is still in state doing
%% it will ignore this message
%% if the fms receives an expire event while in state initiated it will expire</p>
%% @end
%%--------------------------------------------------------------------
-spec(started(Event :: term(), State :: atom()) ->
  {next_state, NextStateName :: atom(), NextState :: #periodic_state{}} |
  {next_state, NextStateName :: atom(), NextState :: #periodic_state{},
    timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #periodic_state{}}).

%% if the maximum number has been reached or a stop message has been sent
started(do, State = #periodic_state{stop = true}) ->
  {stop, normal, State};
started(do, State = #periodic_state{number_of_calls = Max, max = Max}) ->
  error_logger:info_msg("N STp~w ~n", [State]),
  {stop, normal, State};
started(do, State = #periodic_state{number_of_calls = N, task = Task, interval = Time, max = Max}) ->
  try
    NewPeriodicState = Task(State),
    error_logger:info_msg("N MAAX ~w ~n", [[Max, N]]),
    gen_fsm:send_event_after(Time, do),
    {next_state, started, NewPeriodicState#periodic_state{number_of_calls = N + 1}}
  catch
    X:Y -> error_logger:info_msg("Error ~w ~n", [[X, Y,erlang:get_stacktrace()]]),
      {stop, normal, State}
  end.

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
    StateData :: #periodic_state{}) ->
  {next_state, NextStateName :: atom(), NewStateData :: #periodic_state{}} |
  {next_state, NextStateName :: atom(), NewStateData :: #periodic_state{},
    timeout() | hibernate} |
  {stop, Reason :: term(), NewStateData :: #periodic_state{}}).
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
  Reply = {ok, {StateName, State}},
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
|term(), StateName :: atom(), StateData :: term()) -> term()).
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
    StateData :: #periodic_state{}, Extra :: term()) ->
  {ok, NextStateName :: atom(), NewStateData :: #periodic_state{}}).
code_change(_OldVsn, StateName, State, _Extra) ->
  {ok, StateName, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

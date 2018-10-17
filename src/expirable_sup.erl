%%%-------------------------------------------------------------------
%%% @author james
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%% This is the supervisor of the expirable fsm, the name of the fsm
%%% is generated here so that there is no chance of generating a fsm of the same name
%%% @end
%%% Created : 15. May 2016 1:25 PM
%%%-------------------------------------------------------------------
-module(expirable_sup).
-author("james").

-behaviour(supervisor).

-include("../include/periodic_app_common.hrl").

%% API
-export([start_link/0, start_link/1,start_expirable/1,start_expirable_custom_name/1]).

%% Supervisor callbacks
-export([init/1]).


-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link() ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
  supervisor:start_link({global, ?SERVER}, ?MODULE, []).

-spec(start_link(Name :: atom()) ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(Name) ->
  supervisor:start_link({global, Name}, ?MODULE, []).
%% %% @doc this will do the expirable task and end in state doing task
%% -spec(do_task() -> ok |{error, Message :: term()} )).
%% do_task(RegName) ->
%%  ok.

%%--------------------------------------------------------------------
%% @doc
%% Starts a the expirable fsm with state provided as record
%% registerd name is generated here so that there is no chance of generating a fsm of the same name
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_expirable(ParameterAsState :: #expirable_state{}) -> {ok, pid()} | ignore | {error, Reason :: term()}).
start_expirable(ParameterAsState)->
  %% Don’t use dynamic atoms! Atoms go in a global table and are cached forever
  Name = periodic_util:get_guid_number(),
  Restart = transient,
  Shutdown = 2000,
  Type = worker,
  Expirable_fsm = {Name, {expirable_fsm, start_link_sup, [ParameterAsState#expirable_state{name = Name}]},
    Restart, Shutdown, Type, [expirable_fsm]},
  SupervisorName = periodic_env_utiility:expirable_sup_name(),
  Reply = supervisor:start_child({global, SupervisorName}, Expirable_fsm),

  case Reply of
    {ok,Pid} ->{ok,Pid,Name};
    Else -> Else
  end
.

%%--------------------------------------------------------------------
%% @doc
%% Starts a the expirable fsm with state provided as record
%% registerd name is generated here so that there is no chance of generating a fsm of the same name
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_expirable_custom_name(ParameterAsState :: #expirable_state{}) -> {ok, pid()} | ignore | {error, Reason :: term()}).
start_expirable_custom_name(ParameterAsState = #expirable_state{name = Name})->
  %% Don’t use dynamic atoms! Atoms go in a global table and are cached forever
  Restart = transient,
  Shutdown = 2000,
  Type = worker,
  Expirable_fsm = {Name, {expirable_fsm, start_link_sup, [ParameterAsState]},
    Restart, Shutdown, Type, [expirable_fsm]},
  SupervisorName = periodic_env_utiility:expirable_sup_name(),
  Reply = supervisor:start_child({global, SupervisorName}, Expirable_fsm),
  case Reply of
    {ok,Pid} ->{ok,Pid,Name};
    Else -> Else
  end
.


%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%%
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
  {ok, {SupFlags :: {RestartStrategy :: supervisor:strategy(),
    MaxR :: non_neg_integer(), MaxT :: non_neg_integer()},
    [ChildSpec :: supervisor:child_spec()]
  }} |
  ignore |
  {error, Reason :: term()}).
init([]) ->
  RestartStrategy = one_for_one,
  MaxRestarts = 1000,
  MaxSecondsBetweenRestarts = 3600,

  SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

%%   Restart = permanent,
%%   Shutdown = 2000,
%%   Type = worker,
%%
%%   AChild = {'AName', {'AModule', start_link, []},
%%     Restart, Shutdown, Type, ['AModule']},
%%
%%   {ok, {SupFlags, [AChild]}}.


  {ok, {SupFlags, []}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

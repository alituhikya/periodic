%%%-------------------------------------------------------------------
%%% @author alituhikyaj
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 01. Feb 2015 8:49 AM
%%%-------------------------------------------------------------------
-module(periodic_top_sup).
-author("alituhikyaj").

-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, {global, ?MODULE}).
-include("../include/periodic_app_common.hrl").

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%% it expects a list of child supervisors
%% @end
%%--------------------------------------------------------------------
-spec(start_link(Supervisors :: list()) ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(Supervisors) ->
  SupervisorName = periodic_env_utiility:top_sup_name(),
  supervisor:start_link({global,SupervisorName}, ?MODULE, Supervisors).

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
%% this also starts web server
%% it expects a list of supervisors to start
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
  {ok, {SupFlags :: {RestartStrategy :: supervisor:strategy(),
    MaxR :: non_neg_integer(), MaxT :: non_neg_integer()},
    [ChildSpec :: supervisor:child_spec()]
  }} |
  ignore |
  {error, Reason :: term()}).
init( Supervisors) ->
  RestartStrategy = one_for_one,
  MaxRestarts = 1000,
  MaxSecondsBetweenRestarts = 3600,

  SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},


  {ok, {SupFlags, Supervisors}}.


%%%===================================================================
%%% Internal functions
%%%===================================================================
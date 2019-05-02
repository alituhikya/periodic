%%%-------------------------------------------------------------------
%%% @author alituhikyaj
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 01. Feb 2015 8:15 AM
%%%-------------------------------------------------------------------
-module(periodic_app).
-author("alituhikyaj").

-behaviour(application).

-include("../include/periodic_app_common.hrl").
%% Application callbacks
-export([start/2,
  stop/1]).

-define(APPS, []).


%%%===================================================================
%%% Application callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called whenever an application is started using
%% application:start/[1,2], and should start the processes of the
%% application. If the application is structured according to the OTP
%% design principles as a supervision tree, this means starting the
%% top supervisor of the tree.
%%
%% @end
%%--------------------------------------------------------------------
-spec(start(StartType :: normal | {takeover, node()} | {failover, node()},
    StartArgs :: term()) ->
  {ok, pid()} |
  {ok, pid(), State :: term()} |
  {error, Reason :: term()}).
start(normal, []) ->
 start_app();
start({takeover, _OtherNode}, []) ->
  start_app().

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function will be called to start the app, it will start two supervisors
%% the names other supervisors by default are expirable_sup and periodic_sup which can
%% be changed throught the environment variables
%%
%% @end
%%--------------------------------------------------------------------
start_app()->
  io:format("starting.....~n"),
  [begin Start = application:start(A), io:format("~p~n", [Start]), io:format("~p~n", [A]) end || A <- ?APPS],
  Restart = permanent,
  Shutdown = 2000,

  TypeSuper = supervisor,
  EXPIRABLE_SUPERVISOR_NAME = periodic_env_utiility:expirable_sup_name(),
  PERIODIC_SUP_NAME = periodic_env_utiility:periodic_sup_name(),

  ExpirableSup = {EXPIRABLE_SUPERVISOR_NAME, {expirable_sup, start_link, [EXPIRABLE_SUPERVISOR_NAME]},
    Restart, Shutdown, TypeSuper, [expirable_sup]},
  PeriodicSup = {PERIODIC_SUP_NAME, {periodic_sup, start_link, [PERIODIC_SUP_NAME]},
    Restart, Shutdown, TypeSuper, [periodic_sup]},
  case periodic_top_sup:start_link( [ExpirableSup, PeriodicSup]) of
    {ok, Pid} ->
      {ok, Pid};
    Error ->
      Error
  end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called whenever an application has stopped. It
%% is intended to be the opposite of Module:start/2 and should do
%% any necessary cleaning up. The return value is ignored.
%%
%% @end
%%--------------------------------------------------------------------
-spec(stop(State :: term()) -> term()).
stop(_State) ->
  [begin Start = application:stop(A), io:format("~p~n", [Start]), io:format("~p~n", [A]) end || A <- ?APPS],
  ok.




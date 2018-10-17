%%%-------------------------------------------------------------------
%%% @author james
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 23. Feb 2016 5:26 PM
%%%-------------------------------------------------------------------
-module(periodic_env_utiility).
-author("james").

-include("../include/periodic_app_common.hrl").
%% API
-export([
  expirable_sup_name/0,
  periodic_sup_name/0,
  top_sup_name/0
]).
expirable_sup_name() ->
  application:get_env(periodic, expirable_supervisor_name,expirable_sup).

periodic_sup_name() ->
  application:get_env(periodic, periodic_supervisor_name,periodic_sup).


top_sup_name() ->
  application:get_env(periodic, top_supervisor_name,top_sup).


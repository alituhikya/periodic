%%%-------------------------------------------------------------------
%%% @author james
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 18. Oct 2018 12:48 AM
%%%-------------------------------------------------------------------
-module(periodic_util).
-author("james").
-include("../include/periodic_app_common.hrl").
%% API
-export([get_guid_number/0]).


get_guid_number()->
  {MegaSecs,Secs,MicroSecs} = os:timestamp(),
  TimeString = integer_to_list(MegaSecs) ++integer_to_list(Secs)++integer_to_list(MicroSecs),
  list_to_binary(get_random_string(?GUID_LENGTH, ?VERIFY_CHARACTERS)++ TimeString).

get_random_string(Length, AllowedChars) ->
  lists:foldl(fun(_, Acc) ->
    <<A:32, B:32, C:32>> = crypto:rand_bytes(12),
    random:seed(A,B,C),
    [lists:nth(random:uniform(length(AllowedChars)),
      AllowedChars)]
    ++ Acc
              end, [], lists:seq(1, Length)).
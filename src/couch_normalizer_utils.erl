-module(couch_normalizer_utils).
%
%  Utilities for working normalization flow
%
-include("couch_db.hrl").
-export([next_scenario/2, increase_current/1, replace_rev_history_list/2]).



increase_current(Normpos) when is_binary(Normpos) ->
  increase_current(list_to_integer(binary_to_list(Normpos)));

increase_current(Normpos) when is_integer(Normpos) ->
  Normpos + 1.


next_scenario(Ets, Normpos) when is_list(Normpos) ->
  next_scenario(Ets, list_to_integer(Normpos));

next_scenario(Ets, Normpos) when is_binary(Normpos) ->
  next_scenario(Ets, binary_to_list(Normpos));

next_scenario(Ets, Normpos) when is_integer(Normpos) ->
  case ets:next(Ets, Normpos) of
    '$end_of_table' ->
      nil;
    Key -> [H|_] =
      ets:lookup(Ets, Key), H
  end.


replace_rev_history_list(Body, RevHistory) ->
  'Elixir-HashDict':put(Body,<<"rev_history_">>, rev_history_list(RevHistory)).

rev_history_list({Title, Normpos} = _RevHistory) ->
  {[{<<"title">>, Title},{<<"normpos">>, Normpos}]}.
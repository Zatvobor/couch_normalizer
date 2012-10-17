-module(couch_normalizer_util).

-export([next_scenario/1, next_scenario/2, current_registry/0]).



current_registry() ->
  {ok, ScenariosEts} = application:get_env(couch_normalizer_manager, registry),
  ScenariosEts.


next_scenario(Normpos) ->
  next_scenario(current_registry(), Normpos).


next_scenario(Ets, Normpos) when is_list(Normpos) ->
  next_scenario(Ets, list_to_binary(Normpos));


next_scenario(Ets, Normpos) ->
  case ets:next(Ets, Normpos) of
    '$end_of_table' ->
      nil;
    Key -> [H|_] =
      ets:lookup(Ets, Key), H
  end.
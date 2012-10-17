-module(couch_normalizer_util).

-export([document_object/2, document_body/2, current_registry/0, next_scenario/1, next_scenario/2]).



document_object(DbName, DocInfoOrId) ->
  {ok, Db} = couch_db:open_int(DbName, []),
  {ok, Doc} = couch_db:open_doc(Db, DocInfoOrId),
  {Body}    = couch_doc:to_json_obj(Doc, []),

  Id = couch_util:get_value(<<"_id">>, Body),
  Rev = couch_util:get_value(<<"_rev">>, Body),
  Normpos = couch_util:get_value(<<"normpos_">>, Body, <<"0">>),

  {Body, Id, Rev, Normpos}.


document_body(DbName, DocInfoOrId) ->
  {Body, _, _, _} = document_object(DbName, DocInfoOrId), Body.


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
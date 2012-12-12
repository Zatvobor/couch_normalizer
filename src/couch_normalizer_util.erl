-module(couch_normalizer_util).

-export([document_object/2, document_body/2, current_registry/0, next_scenario/1, next_scenario/2, mark_as_deleted/1, update_doc/2]).



document_object(DbName, DocInfoOrId) ->
  {ok, Db} = couch_db:open_int(DbName, []),

  case couch_db:open_doc(Db, DocInfoOrId) of
    {ok, Doc} ->
      {Body}    = couch_doc:to_json_obj(Doc, []),

      Id = couch_util:get_value(<<"_id">>, Body),
      Rev = couch_util:get_value(<<"_rev">>, Body),
      Normpos = couch_util:get_value(<<"normpos_">>, Body, <<"0">>),

      {Body, Id, Rev, Normpos};
    _ -> not_found
  end.


document_body(DbName, DocInfoOrId) ->
  case document_object(DbName, DocInfoOrId) of
    {Body, _, _, _} -> Body;
    _ -> not_found
  end.

mark_as_deleted(Body) ->
  Body ++ [{<<"_deleted">>, true}].

update_doc(DbName, Body) ->
  {ok, Db} = couch_db:open_int(DbName, []),
  couch_db:update_doc(Db, couch_doc:from_json_obj({Body}),[]).

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
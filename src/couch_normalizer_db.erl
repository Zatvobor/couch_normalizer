-module(couch_normalizer_db).
%
%  Utilities for reading/updating documents using a CouchDB functions
%
-include("couch_db.hrl").
-export([document_object/2, document_body/2, update_doc/2]).


document_object(DbName, DocInfoOrId) ->
  {ok, Db} = couch_db:open_int(DbName, []),

  State = case couch_db:open_doc(Db, DocInfoOrId) of
    {ok, Doc} ->
      {Body}       = couch_doc:to_json_obj(Doc, []),
      {RevHistory} = couch_util:get_value(<<"rev_history_">>, Body, {[{<<"normpos">>, 0}]}),

      Id      = couch_util:get_value(<<"_id">>, Body),
      Rev     = couch_util:get_value(<<"_rev">>, Body),
      Normpos = couch_util:get_value(<<"normpos">>, RevHistory, 0),

      {Body, Id, Rev, Normpos};
    _ ->
      not_found
  end,

  couch_db:close(Db),
  State.


document_body(DbName, DocInfoOrId) ->
  case document_object(DbName, DocInfoOrId) of
    {Body, _, _, _} -> Body;
    _ -> not_found
  end.


update_doc(_DbName, not_found) ->
  not_found;

update_doc(DbName, Body) when is_binary(DbName) and is_tuple(Body) ->
  update_doc(DbName, Body:to_list());

update_doc(DbName, Body) when is_binary(DbName) and is_list(Body) ->
  {ok, Db} = couch_db:open_int(DbName, []),
  Status = update_doc(Db, Body),
  couch_db:close(Db),
  Status;

update_doc(Db, Body) when is_record(Db, db) and is_tuple(Body) ->
  update_doc(Db, Body:to_list());

update_doc(Db, Body) when is_record(Db, db) and is_list(Body) ->
  try couch_db:update_doc(Db, couch_doc:from_json_obj({Body}), []) of
    {ok, _}  -> ok
  catch
    conflict -> conflict
  end.
-module(couch_normalizer_process).
-behaviour(supervisor).

-include("couch_db.hrl").
-include("couch_normalizer.hrl").

-export([start_link/1, init/1, terminate/1, spawn_worker/1]).



start_link(Scope) ->
  supervisor:start_link(?MODULE, Scope).


init(S) ->
  DbName = atom_to_binary(S#scope.label, utf8),
  % spawn producer
  spawn(fun() ->
    {ok, Db} = couch_db:open_int(DbName, []),

    Fun = fun(FullDocInfo, _, Acc) ->
      % move given document into the processing_queue
      ok = couch_work_queue:queue(S#scope.processing_queue, {DbName, Db, FullDocInfo}),
      % update status
      gen_server:cast(S#scope.processing_status, {increment_status_param, docs_read}),

      {ok, Acc}
    end,

    % close
    ok = couch_db:close(Db),

    % iterate through each document
    {ok, _, _} = couch_db:enum_docs(Db, Fun, [], [])
  end),

  Child = [{worker, {?MODULE, spawn_worker, [S]}, transient, 1000, worker, dynamic}],
  {ok, {{simple_one_for_one, 10, 3600}, Child}}.


terminate(S) ->
  catch exit(S#scope.processing_sup, normal),
  catch exit(S#scope.processing_status, normal),
  {ok, ternimated}.


spawn_worker(Scope) ->
  {ok, spawn_link(fun() -> worker_loop(Scope) end)}.



%%
%% private
%%

worker_loop(S) ->
  case couch_work_queue:dequeue(S#scope.processing_queue, 1) of
    % apply scenarios for given DocInfo
    {ok, [DocInfo]} ->
      enum_scenarions(S, DocInfo),
      worker_loop(S);
    % stopped processing in case when processing_queue closed
    closed -> stoped
  end.


enum_scenarions(S, {DbName, _, FullDocInfo} = DocInfo) ->
  DocObject = couch_normalizer_util:document_object(DbName, FullDocInfo),
  ok = apply_scenario(S, DocInfo, DocObject).


apply_scenario(_S, _DocInfo, not_found) -> ok;

apply_scenario(S, {DbName, Db, FullDocInfo}, {Body, Id, Rev, CurrentNormpos}) ->
  % find next scenario according to last normpos_ position (or start from the beginning)
  case couch_normalizer_util:next_scenario(S#scope.scenarios_ets, CurrentNormpos) of
    {Normpos, Title, Scenario} ->
      case Scenario(DbName, Id, Rev, Body) of
        {update, NewBody} ->
            ?LOG_INFO("normalize '~p' document according to '~s' scenario~n", [Id, Title]),
            % update normpos
            NormalizedBody = {proplists:delete(<<"normpos_">>, NewBody) ++ [{<<"normpos_">>, Normpos}]},

            % update doc
            {ok, _} = couch_db:update_doc(Db, couch_doc:from_json_obj(NormalizedBody), []),
            % update status
            gen_server:cast(S#scope.processing_status, {increment_status_param, docs_normalized}),
            % release Db
            couch_db:close(Db),
            % try again to apply other scenarions for that document
            ok = enum_scenarions(S, {DbName, Db, Id});
        _ ->
            % increase the current normpos value and try to apply for the current document
            Int = list_to_integer(binary_to_list(CurrentNormpos)),
            NextNormpos = list_to_binary(integer_to_list(Int + 1)),

            ok = apply_scenario(S, {DbName, Db, FullDocInfo}, {Body, Id, Rev, NextNormpos})
      end;
    nil -> ok
  end.

-module(couch_normalizer_process).
%
% Module responsible for doing the normalization
%

-behaviour(supervisor).

-include("couch_db.hrl").
-include("couch_normalizer.hrl").

-export([start_link/1, init/1, terminate/1, spawn_worker/1]).



start_link(Scope) ->
  supervisor:start_link(?MODULE, Scope).


init(#scope{processing_status=undefined} = S) ->
  % starts monitoring
  {ok, SPid} = couch_normalizer_status:start_link(S),
  % continues initialization
  init(S#scope{processing_status=SPid});

init(S) ->
  % spawns linked producer.
  spawn_link(fun() ->
    DbName = atom_to_binary(S#scope.label, utf8),
    {ok, Db} = couch_db:open_int(DbName, []),

    Enum = fun(FullDocInfo, _, Acc) ->
      % enqueues each document
      ok = couch_work_queue:queue(S#scope.processing_queue, {DbName, Db, FullDocInfo}),
      gen_server:cast(S#scope.processing_status, {increment_value, docs_read}),

      {ok, Acc}
    end,

    {ok, _, _} = couch_db:enum_docs(Db, Enum, [], []),
    gen_server:cast(S#scope.processing_status, {update_status, [{continue, false}, {finished_on, oauth_unix:timestamp()}]}),

    ok = couch_db:close(Db)
  end),

  Child = [{worker, {?MODULE, spawn_worker, [S]}, transient, 1000, worker, dynamic}],
  {ok, {{simple_one_for_one, 10, 3600}, Child}}.


terminate(S) ->
  catch exit(S#scope.processing_sup, shutdown),
  {ok, realsed}.


spawn_worker(Scope) ->
  {ok, spawn_link(fun() -> worker_loop(Scope) end)}.


worker_loop(S) ->
  case couch_work_queue:dequeue(S#scope.processing_queue, 1) of
    % applies scenario
    {ok, [DocInfo]} ->
      enum_scenarions(S, DocInfo),
      worker_loop(S);
    % stops processing in case when processing_queue closed
    closed -> stoped
  end.


enum_scenarions(S, {DbName, _, FullDocInfo} = DocInfo) ->
  DocObject = couch_normalizer_utils:document_object(DbName, FullDocInfo),
  ok = apply_scenario(S, DocInfo, DocObject).


apply_scenario(_S, _DocInfo, not_found) -> ok;

apply_scenario(S, {DbName, Db, FullDocInfo}, {Body, Id, Rev, CurrentNormpos}) ->
  % finds the next scenario according to the last normpos_ position (or start apply scenarios from the beginning)
  case couch_normalizer_utils:next_scenario(S#scope.scenarios_ets, CurrentNormpos) of
    {Normpos, Title, ScenarioFun} ->
      case 'Elixir-CouchNormalizer-Scenario':call(ScenarioFun, {DbName, Id, Rev, Body}) of
        {update, NewBody} ->
            ?LOG_INFO("normalize '~p' document according to '~s' scenario~n", [Id, Title]),

            % updates rev_history_ field
            NormalizedBody = {couch_normalizer_utils:replace_rev_history_list(NewBody, {Title, Normpos})},
            % updates modified document
            {ok, _} = couch_db:update_doc(Db, couch_doc:from_json_obj(NormalizedBody), []),
            % updates execution status
            gen_server:cast(S#scope.processing_status, {increment_value, docs_normalized}),
            couch_db:close(Db),

            % tries again to apply another scenarios for that document
            ok = enum_scenarions(S, {DbName, Db, Id});
        _ ->
            % increases the current normpos value and try to find the next scenario
            NextNormpos = couch_normalizer_utils:increase_current(CurrentNormpos),
            ok = apply_scenario(S, {DbName, Db, FullDocInfo}, {Body, Id, Rev, NextNormpos})
      end;
    nil -> ok
  end.
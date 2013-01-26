-module(couch_normalizer_process).
%
% This module is responsible for doing the normalization.
%
-behaviour(supervisor).

-include("couch_db.hrl").
-include("couch_normalizer.hrl").

-export([start_link/1, init/1, terminate/1, spawn_worker/1]).


start_link(Scope) ->
  supervisor:start_link(?MODULE, Scope).


init(#scope{processing_status=undefined} = S) ->
  % starts status monitoring server
  {ok, Pid} = couch_normalizer_status:start_link(S),

  init(S#scope{processing_status=Pid});

init(#scope{scenarios_ets=undefined} = S) ->
  % acquires (load) normalization scenarios into registry
  'Elixir-CouchNormalizer-Registry':init(),
  'Elixir-CouchNormalizer-Registry':load_all(S#scope.scenarios_path),

  % aquires loaded scenarions back to the process
  ScenariosEts = 'Elixir-CouchNormalizer-Registry':to_ets(),

  init(S#scope{scenarios_ets=ScenariosEts});

init(#scope{processing_queue=undefined} = S) ->
  % setups processing queue options
  {ok, ProcessingQueue} = couch_work_queue:new([{multi_workers, true}]),

  init(S#scope{processing_queue = ProcessingQueue});

init(S) ->
  % spawns document reader process
  spawn_link(fun() ->
    DbName = atom_to_binary(S#scope.label, utf8),
    {ok, Db} = couch_db:open_int(DbName, []),

    QueueFun = fun(FullDocInfo, _, Acc) ->
      couch_work_queue:queue(S#scope.processing_queue, {DbName, Db, FullDocInfo}),
      couch_normalizer_status:increment_docs_read(S#scope.processing_status),

      {ok, Acc}
    end,

    couch_db:enum_docs(Db, QueueFun, [], []),

    couch_db:close(Db),
    couch_work_queue:close(S#scope.processing_queue),

    couch_normalizer_status:update_continue_false(S#scope.processing_status)
  end),

  ChildSpec = [{worker, {?MODULE, spawn_worker, [S]}, transient, 1000, worker, dynamic}],
  {ok, {{simple_one_for_one, 10, 3600}, ChildSpec}}.


terminate(S) ->
  catch exit(S#scope.processing_sup, shutdown),
  {ok, terminated}.


spawn_worker(Scope) ->
  {ok, spawn_link(fun() -> worker_loop(Scope) end)}.


worker_loop(S) ->
  case couch_work_queue:dequeue(S#scope.processing_queue, 1) of
    {ok, [DocInfo]} ->
      apply_scenario(S, DocInfo),
      worker_loop(S);
    closed -> stoped
  end.


apply_scenario(S, {DbName, _, FullDocInfo} = DocInfo) ->
  DocObject = couch_normalizer_utils:document_object(DbName, FullDocInfo),
  ok = apply_scenario(S, DocInfo, DocObject).

apply_scenario(_S, _DocInfo, not_found) ->
  ok;

apply_scenario(S, {DbName, Db, FullDocInfo}, {Body, Id, Rev, CurrentNormpos}) ->
  % finds the next scenario according to the last normpos_ position
  % or try to start from the beginning
  case couch_normalizer_utils:next_scenario(S#scope.scenarios_ets, CurrentNormpos) of
    {Normpos, Title, ScenarioFun} ->
      case 'Elixir-CouchNormalizer-Scenario':call(ScenarioFun, {DbName, Id, Rev, Body}) of
        {update, BodyDict} ->
          % saves changes for certain document and resolve a conflict
          ok = apply_changes(S, {DbName, Db}, {BodyDict, Id}, {Normpos, Title});
        _ ->
          % increases the current normpos value and try to find the next scenario
          NextNormpos = couch_normalizer_utils:increase_current(CurrentNormpos),
          ok = apply_scenario(S, {DbName, Db, FullDocInfo}, {Body, Id, Rev, NextNormpos})
      end;
    % no more available scenarios
    % so, goes to the next document
    nil -> ok
  end.


apply_changes(S, {DbName, Db}, {BodyDict, Id}, {Normpos, Title}) ->
  % updates rev_history_ field
  RevHistoryBodyDict = couch_normalizer_utils:replace_rev_history_list(BodyDict, {Title, Normpos}),
  % tries to update document.
  case couch_normalizer_utils:update_doc(DbName, RevHistoryBodyDict) of
    ok ->
      ?LOG_INFO("'~p' normalized according to '~s' scenario~n", [Id, Title]),
      % updates execution status
      gen_server:cast(S#scope.processing_status, {increment_value, docs_normalized}),
      % tries again to apply another scenarios for that document
      ok = apply_scenario(S, {DbName, Db, Id});
    conflict ->
      ?LOG_INFO("conflict occured for '~p' during processing a '~s' scenario~n", [Id, Title]),
      gen_server:cast(S#scope.processing_status, {increment_value, docs_conflicted})
  end,

  couch_db:close(Db),
  ok.
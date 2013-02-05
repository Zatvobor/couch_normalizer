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
      gen_server:cast(S#scope.processing_status, {increment_value, docs_read}),

      {ok, Acc}
    end,

    couch_db:enum_docs(Db, QueueFun, [], []),

    couch_db:close(Db),
    couch_work_queue:close(S#scope.processing_queue),

    gen_server:cast(S#scope.processing_status, {update_status, [{continue, false}, {finished_on, oauth_unix:timestamp()}]})
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

    closed ->
      stoped
  end.


apply_scenario(S, {DbName, _, FullDocInfo} = DocInfo) ->
  DocObject = couch_normalizer_db:document_object(DbName, FullDocInfo),
  ok = apply_scenario(S, DocInfo, DocObject).

apply_scenario(_S, _DocInfo, not_found) ->
  ok;

apply_scenario(S, {DbName, Db, FullDocInfo}, {Body, Id, Rev, CurrentNormpos}) ->
  % finds the next scenario according to the last normpos_ position
  % or try to start from the beginning
  case next_scenario(S#scope.scenarios_ets, CurrentNormpos) of
    {Normpos, Title, ScenarioFun} ->
      case 'Elixir-CouchNormalizer-Scenario':call(ScenarioFun, {DbName, Id, Rev, Body}) of
        {update, BodyDict} ->
          % saves changes for certain document and resolve a conflict
          ok = apply_changes(S, {DbName, Db}, {BodyDict, Id}, {Normpos, Title});
        _ ->
          % increases the current normpos value and try to find the next scenario
          NextNormpos = increase_current(CurrentNormpos),
          ok = apply_scenario(S, {DbName, Db, FullDocInfo}, {Body, Id, Rev, NextNormpos})
      end;
    % no more available scenarios
    % so, goes to the next document
    nil -> ok
  end.


apply_changes(S, {DbName, Db}, {BodyDict, Id}, {Normpos, Title}) ->
  % puts in updates to a 'rev_history_' field
  RevHistoryBodyDict = replace_rev_history_list(BodyDict, {Title, Normpos}),
  case couch_normalizer_db:update_doc(Db, RevHistoryBodyDict) of
    ok ->
      ?LOG_INFO("'~p' normalized according to '~s' scenario~n", [Id, Title]),
      gen_server:cast(S#scope.processing_status, {increment_value, docs_normalized}),
      % tries again to apply another scenarios for that document
      ok = apply_scenario(S, {DbName, Db, Id});
    conflict ->
      ?LOG_INFO("conflict occured for '~p' during processing a '~s' scenario~n", [Id, Title]),
      gen_server:cast(S#scope.processing_status, {increment_value, docs_conflicted}),
      % it's ok, does nothing. So, go to the next document
      ok
  end.


% internal helpers for processing flow

next_scenario(Ets, Normpos) when is_list(Normpos) ->
  next_scenario(Ets, list_to_integer(Normpos));

next_scenario(Ets, Normpos) when is_binary(Normpos) ->
  next_scenario(Ets, binary_to_list(Normpos));

next_scenario(Ets, Normpos) when is_integer(Normpos) ->
  case ets:next(Ets, Normpos) of
    '$end_of_table' ->
      nil;
    Key ->
      [H|_] = ets:lookup(Ets, Key),
      H
  end.


increase_current(Normpos) when is_binary(Normpos) ->
  increase_current(list_to_integer(binary_to_list(Normpos)));

increase_current(Normpos) when is_integer(Normpos) ->
  Normpos + 1.


replace_rev_history_list(Body, RevHistory) ->
  'Elixir-HashDict':put(Body,<<"rev_history_">>, rev_history_list(RevHistory)).


rev_history_list({Title, Normpos} = _RevHistory) ->
  {[{<<"title">>, Title},{<<"normpos">>, Normpos}]}.
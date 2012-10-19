-module(couch_normalizer_manager).
-behaviour(gen_server).

-include("couch_db.hrl").

% public API
-export([start_link/1]).

% gen_server callbacks
-export([init/1, terminate/2]).
-export([handle_call/3]). %, handle_cast/2, code_change/3, handle_info/2]).


-record(scope, {
    % common processing scope definition
    label,
    scenarios_path = undefined,
    num_workers,
    qmax_items,
    % dynamic properties, unique for each processing flow
    scenarios_ets,
    processing_queue
}).


start_link(Config) ->

  % configure
  F = fun({Label, Options} = _E) ->
    Scope = #scope {
      label = Label,
      scenarios_path = couch_util:get_value(scenarios_path, Options, "/usr/local/etc/couchdb/scenarions"),
      num_workers    = couch_util:get_value(num_workers, Options, 3),
      qmax_items     = couch_util:get_value(qmax_items, Options, 100)
    },
    {Label, Scope}
  end,

  State = lists:map(F, Config),

  % setup registry
  application:start(elixir),

  % spawn server instance
  {ok, _} = gen_server:start_link({local, ?MODULE}, ?MODULE, State, []).


%
% gen_server callbacks
%

init(State) -> {ok, State}.

terminate(_Reason, _State) -> ok.

handle_call({normalize, DbName}, _From, State) ->
  case proplists:lookup(binary_to_atom(DbName, utf8), State) of
    {_, Scope} ->
      application:set_env(?MODULE, registry, ets:new(s, [ordered_set, {keypos,1}])),

      % compile and load scenarios
      'Elixir-CouchNormalizer-Registry':load_all(Scope#scope.scenarios_path),

      % move aquired scenarions back to the given scope
      ScenariosEts          = couch_normalizer_util:current_registry(),
      {ok, ProcessingQueue} = couch_work_queue:new([{max_items, Scope#scope.qmax_items}, {multi_workers, true}]),

      spawn_processing(Scope#scope{scenarios_ets = ScenariosEts, processing_queue = ProcessingQueue}, DbName),

      {reply, ok, State};
    none ->
      {reply, {error, "Skipped. Can't find requested scope."}, State}
  end.

  % couch_task_status:add_task([
  %     {type, replication},
  %     {replication_id, ?l2b(BaseId ++ Ext)},
  %     {doc_id, Rep#rep.doc_id},
  %     {source, ?l2b(SourceName)},
  %     {target, ?l2b(TargetName)},
  %     {continuous, get_value(continuous, Options, false)},
  %     {revisions_checked, 0},
  %     {missing_revisions_found, 0},
  %     {docs_read, 0},
  %     {docs_written, 0},
  %     {doc_write_failures, 0},
  %     {source_seq, SourceCurSeq},
  %     {checkpointed_source_seq, CommittedSeq},
  %     {progress, 0}
  % ]),
  % couch_task_status:set_update_frequency(1000),



%%
%% private
%%

spawn_processing(S, DbName) ->
  % spawn producer
  spawn(fun() ->
    {ok, Db} = couch_db:open_int(DbName, []),

    Fun = fun(FullDocInfo, _, Acc) ->
      % move given document into the processing_queue
      ok = couch_work_queue:queue(S#scope.processing_queue, {DbName, Db, FullDocInfo}),

      {ok, Acc}
    end,

    % close
    ok = couch_db:close(Db),

    % iterate through each document
    {ok, _, _} = couch_db:enum_docs(Db, Fun, [], [])
  end),

  % spawn workers
  lists:map(fun(_) -> spawn(fun() -> worker_loop(S) end) end, lists:seq(1, S#scope.num_workers)).


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


apply_scenario(_S, _DocInfo, not_found) ->
  ok;

apply_scenario(S, {DbName, Db, FullDocInfo}, {Body, Id, Rev, CurrentNormpos}) ->
  % find next scenario according to last normpos_ position (or start from the beginning)
  case couch_normalizer_util:next_scenario(S#scope.scenarios_ets, CurrentNormpos) of
    {Normpos, _, Scenario} ->
      case Scenario(DbName, Id, Rev, Body) of
        {update, NewBody} ->
            % update normpos
            NormalizedBody = {proplists:delete(<<"normpos_">>, NewBody) ++ [{<<"normpos_">>, Normpos}]},

            % update doc
            {ok, _} = couch_db:update_doc(Db, couch_doc:from_json_obj(NormalizedBody), []),
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
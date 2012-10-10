-module(couch_normalizer_manager).
-behaviour(gen_server).

-include("couch_db.hrl").

% public API
-export([start_link/1, registry/0, next_scenario/1]).
%
% gen_server callbacks
-export([init/1, terminate/2]).
-export([handle_call/3]). %, handle_cast/2, code_change/3, handle_info/2]).


-record(state, {
    scenarios_path = undefined,
    num_workers,
    qmax_items
}).



start_link(Options) ->
  % configure
  State = #state{
    scenarios_path = couch_util:get_value(scenarios_path, Options, "/usr/local/etc/couchdb/scenarions"),
    num_workers    = couch_util:get_value(num_workers, Options, 3),
    qmax_items     = couch_util:get_value(qmax_items, Options, 100)
  },


  % setup registry
  application:start(elixir),
  application:set_env(?MODULE, registry, ets:new(s, [ordered_set, {keypos,1}])),

  % compile and load scenarios
  'Elixir-CouchNormalizer-Registry':load_all(State#state.scenarios_path),

  % spawn server instance
  {ok, _} = gen_server:start_link({local, ?MODULE}, ?MODULE, State, []).


%
% gen_server callbacks
%

init(State) -> {ok, State}.

terminate(_Reason, _State) -> ok.

handle_call({normalize, DbName}, _From, State) ->
  {ok, Q} = couch_work_queue:new([{max_items, State#state.qmax_items}, {multi_workers, true}]),

  spawn_producer(DbName, Q),

  Workers = lists:map(
    fun(_) -> spawn_worker(Q) end,
    lists:seq(1, State#state.num_workers)
  ),

  {reply, Workers, State}.

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


%
% public API
%

registry() ->
  application:get_env(?MODULE, registry).


next_scenario(Normpos) when is_list(Normpos) ->
  next_scenario(list_to_binary(Normpos));

next_scenario(Normpos) ->
  {ok, Pid} = registry(),
  case ets:next(Pid, Normpos) of
    '$end_of_table' ->
      nil;
    Key -> [H|_] =
      ets:lookup(Pid, Key), H
  end.



%
% private
%

spawn_producer(DbName, Q) ->
  spawn(fun() ->
    {ok, Db} = couch_db:open_int(DbName, []),

    Fun = fun(FullDocInfo, _, Acc) ->
      % apply all scenarions for given document
      ok = couch_work_queue:queue(Q, {DbName, Db, FullDocInfo}),
      {ok, Acc}
    end,

    % iterate through each document
    {ok, _, _} = couch_db:enum_docs(Db, Fun, [], []),

    % finish normalization process
    ok = couch_db:close(Db)
  end).


spawn_worker(Q) ->
  Parent = self(),
  spawn(fun() -> worker_loop(Parent, Q) end).


worker_loop(Parent, Q) ->
  case couch_work_queue:dequeue(Q, 1) of
    {ok, [{DbName, Db, FullDocInfo}]} ->
      enum_scenarions(DbName, Db, FullDocInfo),
      worker_loop(Parent, Q);
    closed ->
      stoped
  end.


enum_scenarions(DbName, Db, FullDocInfo) ->
  {ok, Doc} = couch_db:open_doc(Db, FullDocInfo),

  {Body} = couch_doc:to_json_obj(Doc, []),
  Id = couch_util:get_value(<<"_id">>, Body),
  Rev = couch_util:get_value(<<"_rev">>, Body),
  CurrentNormpos = couch_util:get_value(<<"normpos_">>, Body, <<"0">>),

  ok = apply_scenario(DbName, Db, FullDocInfo, Body, Id, Rev, CurrentNormpos).


apply_scenario(DbName, Db, FullDocInfo, Body, Id, Rev, CurrentNormpos) ->
  case next_scenario(CurrentNormpos) of
    {Normpos, _, Scenario} ->
      case Scenario(DbName, Id, Rev, Body) of
        {update, NewBody} ->
            % update normpos
            NormalizedBody = {proplists:delete(<<"normpos_">>, NewBody) ++ [{<<"normpos_">>, Normpos}]},

            % update doc
            {ok, _} = couch_db:update_doc(Db, couch_doc:from_json_obj(NormalizedBody), []),
            ok; % !! enum_scenarions(DbName, Db, Id);
        _ ->
            % increase the current normpos value and try to apply for the current document
            Int = list_to_integer(binary_to_list(CurrentNormpos)),
            NextNormpos = list_to_binary(integer_to_list(Int + 1)),

            ok = apply_scenario(DbName, Db, FullDocInfo, Body, Id, Rev, NextNormpos)
      end;
    nil -> ok
  end.
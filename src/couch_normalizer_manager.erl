-module(couch_normalizer_manager).
-behaviour(gen_server).

-include("couch_db.hrl").
-include("couch_normalizer.hrl").

% public API
-export([start_link/1]).

% gen_server callbacks
-export([init/1, terminate/2]).
-export([handle_call/3]). %, handle_cast/2, code_change/3, handle_info/2]).



start_link(Config) ->
  % map the configuration to the server state
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
  Label = binary_to_atom(DbName, utf8),

  case proplists:get_value(Label, State) of
    undefined ->
      {reply, {error, ?l2b("Skipped. Can't find requested scope definition.")}, State};
    Scope ->
      application:set_env(?MODULE, registry, ets:new(s, [ordered_set, {keypos,1}])),

      % compile and load scenarios
      'Elixir-CouchNormalizer-Registry':load_all(Scope#scope.scenarios_path),

      % move aquired scenarions back to the given scope
      ScenariosEts = couch_normalizer_util:current_registry(),
      {ok, ProcessingQueue} = couch_work_queue:new([{max_items, Scope#scope.qmax_items}, {multi_workers, true}]),

      % stop if it already started and/or spawn the new processing from the beginning
      ProcessingScope = Scope#scope{label = Label, scenarios_ets = ScenariosEts, processing_queue = ProcessingQueue},
      terminate_processing(ProcessingScope),
      {ok, NewScope} = start_processing(ProcessingScope),

      % capture the processing info in server's state
      NewState = proplists:delete(Label, State) ++ [{Label, NewScope}],
      Message = io_lib:format("Normalization process has been started (~p).", [NewScope#scope.processing_sup]),
      {reply, {ok, ?l2b(Message)}, NewState}
  end.


terminate_processing(Scope) ->
  couch_normalizer_process:terminate(Scope).


start_processing(S) ->
  DbName = atom_to_binary(S#scope.label, utf8),
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
  {ok, Pid} = couch_normalizer_process:start_link(S),
  lists:map(fun(_) -> supervisor:start_child(Pid, []) end, lists:seq(1, S#scope.num_workers)),
  {ok, S#scope{processing_sup = Pid}}.

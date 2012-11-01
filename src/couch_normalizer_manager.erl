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
  Action = fun(Label, Scope) ->
    application:set_env(?MODULE, registry, ets:new(s, [ordered_set, {keypos,1}])),

    % compile and load scenarios
    'Elixir-CouchNormalizer-Registry':load_all(Scope#scope.scenarios_path),

    % move aquired scenarions back to the given scope
    ScenariosEts = couch_normalizer_util:current_registry(),
    {ok, ProcessingQueue} = couch_work_queue:new([{max_items, Scope#scope.qmax_items}, {multi_workers, true}]),


    ProcessingScope = Scope#scope{label = Label, scenarios_ets = ScenariosEts, processing_queue = ProcessingQueue},
    % stop if it already started and/or spawn the new processing from the beginning
    terminate_processing(ProcessingScope),
    % start processing flow
    {ok, NewScope} = start_processing(ProcessingScope),

    % capture the processing scope in server's state
    NewState = proplists:delete(Label, State) ++ [{Label, NewScope}],
    Message = io_lib:format("Normalization process has been started (~p).", [NewScope#scope.processing_sup]),
    {reply, {ok, ?l2b(Message)}, NewState}
  end,
  handle_scoped_call(Action, DbName, State);

handle_call({cancel, DbName}, _From, State) ->
  Action = fun(_Label, Scope) ->
    {reply, terminate_processing(Scope), State}
  end,
  handle_scoped_call(Action, DbName, State).

%
% private
%

handle_scoped_call(Action, DbName, State) ->
  Label = binary_to_atom(DbName, utf8),
  case proplists:get_value(Label, State) of
    undefined ->
      {reply, {error, ?l2b("Skipped. Can't find requested scope definition.")}, State};
    Scope ->
      Action(Label, Scope)
  end.


terminate_processing(Scope) ->
  couch_normalizer_process:terminate(Scope).


start_processing(S) ->
  % start monitoring
  {ok, SPid} = couch_normalizer_status:start_link(S),
  Monitorable = S#scope{processing_status=SPid},

  % spawn processing workers
  {ok, NPid} = couch_normalizer_process:start_link(Monitorable),
  lists:map(fun(_) -> supervisor:start_child(NPid, []) end, lists:seq(1, S#scope.num_workers)),

  {ok, Monitorable#scope{processing_sup = NPid}}.

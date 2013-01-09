-module(couch_normalizer_manager).
%
% Module holds the logic responsible for starting and executing a particular
% normalization process.
%

-behaviour(gen_server).

-include("couch_db.hrl").
-include("couch_normalizer.hrl").

-export([start_link/1, init/1, terminate/2, handle_call/3]).



start_link(Config) ->
  % starts dependent applications
  application:start(elixir),

  % a state factory as a callback function
  F = fun({Label, Options} = _E) ->
    Scope = #scope {
      label = Label,
      scenarios_path = couch_util:get_value(scenarios_path, Options, "/usr/local/etc/couchdb/scenarions"),
      num_workers    = couch_util:get_value(num_workers, Options, 3),
      qmax_items     = couch_util:get_value(qmax_items, Options, 100)
    },
    {Label, Scope}
  end,

  % % starts the gen_server
  State = lists:map(F, Config),
  {ok, _} = gen_server:start_link({local, ?MODULE}, ?MODULE, State, []).


%
% normalization request handlers
%
handle_call({normalize, DbName}, _From, State) ->
  Action = fun(Label, Scope) ->
    % setups processing queue options
    {ok, ProcessingQueue} = couch_work_queue:new([{max_items, Scope#scope.qmax_items}, {multi_workers, true}]),

    ProcessingScope = Scope#scope{label = Label, processing_queue = ProcessingQueue},

    % restarts processing flow
    terminate_processing(ProcessingScope),
    {ok, NewScope} = start_processing(ProcessingScope),

    % captures the processing scope in server's state
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
  {ok, Pid} = couch_normalizer_process:start_link(S),
  % see couch_normalizer_process:terminate/1 implementation
  unlink(Pid),
  % spawns processing workers
  Enum = fun(_) ->
    supervisor:start_child(Pid, [])
  end,
  lists:map(Enum, lists:seq(1, S#scope.num_workers)),

  {ok, S#scope{processing_sup = Pid}}.


init(State) ->
  {ok, State}.

terminate(_Reason, _State) ->
  ok.
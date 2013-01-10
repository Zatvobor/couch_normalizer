-module(couch_normalizer_manager).
%
% This module is responsible for managing normalization.
%
-behaviour(gen_server).

-include("couch_db.hrl").
-include("couch_normalizer.hrl").

-export([start_link/1, init/1, terminate/2, handle_call/3, handle_cast/2, handle_info/2, code_change/3]).


start_link(Config) ->
  % starts dependent applications
  application:start(elixir),

  % a state factory as a callback function
  Action = fun({Label, Options} = _E) ->
    Scope = #scope {
      label          = Label,
      scenarios_path = couch_util:get_value(scenarios_path, Options, "/usr/local/etc/couchdb/scenarions"),
      num_workers    = couch_util:get_value(num_workers, Options, 3)
    },
    {Label, Scope}
  end,

  % % starts the gen_server
  State = lists:map(Action, Config),
  {ok, _} = gen_server:start_link({local, ?MODULE}, ?MODULE, State, []).


init(State) ->
  {ok, State}.


handle_call({normalize, DbName}, _From, State) ->
  Action = fun(Label, Scope) ->
    ProcessingScope = Scope#scope{label = Label},

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


terminate_processing(Scope) ->
  couch_normalizer_process:terminate(Scope).


handle_cast(_Request, State) ->
  %% The function is there for the behaviour,
  %% but will not be used. Only a version on the next
  {noreply, State}.


handle_info(_Info, State) ->
  %% The function is there for the behaviour,
  %% but will not be used. Only a version on the next
  {noreply, State}.


code_change(_OldVsn, State, _Extra) ->
  %% The function is there for the behaviour,
  %% but will not be used. Only a version on the next
  {ok, State}.


terminate(_Reason, _State) ->
  %% The function is there for the behaviour,
  %% but will not be used. Only a version on the next
  ok.
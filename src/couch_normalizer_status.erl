-module(couch_normalizer_status).
-behaviour(gen_server).

-include("couch_db.hrl").
-include("couch_normalizer.hrl").

% public API
-export([start_link/1]).
% gen_server callbacks
-export([init/1, terminate/2, handle_cast/2]).



start_link(Scope) ->
  gen_server:start_link(?MODULE, Scope, []).


init(S) ->
  couch_task_status:add_task([
      {type, normalization},
      {db, S#scope.label},
      {num_workers, S#scope.num_workers},
      {continue, true},
      {docs_read, 0},
      {docs_normalized, 0}
  ]),

  % couch_task_status:set_update_frequency(500),

  {ok, stateless}.


handle_cast({increment_value, Param}, State) ->
  [Value] = couch_task_status:get([Param]),
  couch_task_status:update([{Param, Value + 1}]),

  {noreply, State};

handle_cast({update_status, Status}, State) ->
  couch_task_status:update(Status),
  {noreply, State}.


terminate(_Reason, _State) ->
  ok.

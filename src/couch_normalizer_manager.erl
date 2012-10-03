-module(couch_normalizer_manager).
-behaviour(gen_server).


-export([start_link/1, init/1, registry/0, lookup_next/1]).



start_link([{scenario_path, Ph}]) ->
  % 1. setup registry
  application:set_env(?MODULE, registry, ets:new(s, [ordered_set, {keypos,1}])),

  % 2. compile and load scenarios
  'Elixir-CouchNormalizer-Registry':load(Ph),

  % 3. spawn server
  {ok, _} = gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


init(_args) ->
  % 1. init workers pool
  {ok, []}.


registry() ->
  application:get_env(?MODULE, registry).

lookup_next(Normpos) ->
  {ok, Pid} = registry(),
  case ets:next(Pid, Normpos) of
    Key -> [H|T] = ets:lookup(Pid, Key), H;
    '$end_of_table' -> nil
  end.

-module(couch_normalizer).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(normal, _StartArgs) ->
    couch_normalizer_manager:start_link([]).

stop(_State) ->
    ok.

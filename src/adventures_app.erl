-module(adventures_app).

-behaviour(application).

%% Application callbacks
-export([start/0, start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start() -> start([],[]).

start(_StartType, _StartArgs) ->
    adventures_sup:start_link().

stop(_State) ->
    ok.

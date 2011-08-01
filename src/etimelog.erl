-module(etimelog).
-behaviour(application).
-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    etimelog_sup:start_link().

stop(_State) ->
    ok.

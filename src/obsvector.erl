-module(obsvector).

-export([start/0]).

start() ->
    ok = application:start(crypto),
    ok = application:start(ranch),
    ok = application:start(sync),
    ok = application:start(cowlib),
    ok = application:start(cowboy),
    ok = application:start(obsvector),
    sync:go().

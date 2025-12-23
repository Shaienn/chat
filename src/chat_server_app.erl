-module(chat_server_app).
-behaviour(application).

-export([start/0, start/2, stop/1]).

start() ->
    application:ensure_all_started(chat_server).

start(_StartType, _StartArgs) ->
    chat_sup:start_link().

stop(_State) ->
    ok.


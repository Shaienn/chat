-module(chat_sup).
-behaviour(supervisor).

-include("chat.hrl").

-export([start_link/0, start_link/1]).
-export([init/1]).

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    start_link(#{}).

-spec start_link(server_opts()) -> {ok, pid()} | {error, term()}.
start_link(Opts) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [Opts]).

init([Opts]) ->
    %% Создаём ETS таблицы в процессе супервизора — переживут рестарт сервера
    chat_server:create_tables(),
    
    Children = [
        #{
            id => pg,
            start => {pg, start_link, []},
            restart => permanent,
            type => worker
        },
        #{
            id => chat_connection_sup,
            start => {chat_connection_sup, start_link, []},
            restart => permanent,
            type => supervisor
        },
        #{
            id => chat_server,
            start => {chat_server, start_link, [Opts]},
            restart => permanent,
            type => worker
        },
        #{
            id => chat_acceptor,
            start => {chat_acceptor, start_link, [Opts]},
            restart => permanent,
            type => worker
        }
    ],
    
    {ok, {{one_for_one, 10, 10}, Children}}.

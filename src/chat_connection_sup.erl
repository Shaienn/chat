-module(chat_connection_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([start_connection/2]).
-export([init/1]).

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

-spec start_connection(gen_tcp:socket(), map()) -> {ok, pid()} | {error, term()}.
start_connection(Socket, Opts) ->
    supervisor:start_child(?MODULE, [Socket, Opts]).

init([]) ->
    ChildSpec = #{
        id => chat_connection,
        start => {chat_connection, start_link, []},
        restart => temporary,
        shutdown => 5000,
        type => worker,
        modules => [chat_connection]
    },
    {ok, {{simple_one_for_one, 0, 1}, [ChildSpec]}}.


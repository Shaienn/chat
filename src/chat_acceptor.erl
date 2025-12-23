-module(chat_acceptor).

-include("chat.hrl").
-include_lib("kernel/include/logger.hrl").

%% API
-export([start_link/1]).

%% Internal
-export([init/1]).

-spec start_link(server_opts()) -> {ok, pid()}.
start_link(Opts) ->
    Pid = proc_lib:spawn_link(?MODULE, init, [Opts]),
    {ok, Pid}.

init(Opts) ->
    Port = maps:get(port, Opts, ?DEFAULT_PORT),
    {ok, ListenSocket} = gen_tcp:listen(Port, [binary, {packet, 2}, {reuseaddr, true}, {active, false}]),
    accept_loop(ListenSocket, Opts).

accept_loop(ListenSocket, Opts) ->
    case gen_tcp:accept(ListenSocket) of
        {ok, Socket} ->
            ?LOG_INFO("new connection accepted"),
            inet:setopts(Socket, [{keepalive, true}]),
            case chat_connection_sup:start_connection(Socket, Opts) of
                {ok, Pid} ->
                    gen_tcp:controlling_process(Socket, Pid);
                {error, Reason} ->
                    ?LOG_ERROR("failed to start connection: ~p", [Reason]),
                    gen_tcp:close(Socket)
            end,
            accept_loop(ListenSocket, Opts);
        {error, closed} ->
            ?LOG_INFO("listen socket closed, restarting"),
            init(Opts)
    end.

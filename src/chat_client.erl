-module(chat_client).
-behaviour(gen_server).

-include("chat.hrl").
-include_lib("kernel/include/logger.hrl").

%% API
-export([start/2, start/3, start_link/1]).
-export([send_message/1, send_message/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-record(state, {
    username,
    socket,
    event_handler
}).

%% =============================================================================
%% API
%% =============================================================================

-spec start(username() | atom() | string(), password() | atom() | string()) -> 
    {ok, pid()} | {error, term()}.
start(Username, Password) ->
    start(Username, Password, #{}).

-spec start(username() | atom() | string(), password() | atom() | string(), client_opts()) -> 
    {ok, pid()} | {error, term()}.
start(Username, Password, Opts) ->
    connect_and_start(to_bin(Username), to_bin(Password), Opts).

-spec start_link(client_opts()) -> {ok, pid()} | {error, term()}.
start_link(Opts) ->
    case maps:get(test_env, Opts, false) of
        true ->
            % Для тестирования не регистрируем процесс
            gen_server:start_link(?MODULE, [Opts], []);
        false ->
            gen_server:start_link({local, ?MODULE}, ?MODULE, [Opts], [])
    end.

-spec send_message(binary() | atom() | string()) -> ok | {error, empty_message}.
send_message(Msg) ->
    send_message(?MODULE, Msg).

-spec send_message(atom() | pid(), binary() | atom() | string()) -> ok | {error, empty_message}.
send_message(Ref, Msg) ->
    do_send_message(Ref, to_bin(Msg)).

%% =============================================================================
%% gen_server callbacks
%% =============================================================================

init([#{socket := Socket, username := Username} = Opts]) ->
    ok = inet:setopts(Socket, [{active, once}]),
    {ok, #state{socket = Socket, username = Username, event_handler = maps:get(event_handler, Opts, undefined)}}.

handle_call({send_message, MsgRaw}, _From, #state{socket = Socket} = State) ->
    ?LOG_INFO("<-- ~p", [MsgRaw]),
    Msg = make_message(MsgRaw),
    ok = gen_tcp:send(Socket, Msg),
    maybe_send_event({sent_message, MsgRaw}, State),
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({tcp, Socket, <<"MSG", M/binary>>}, #state{socket = Socket} = State) ->
    maybe_send_event({got_message, M}, State),
    ?LOG_INFO("--> ~p", [M]),
    ok = inet:setopts(Socket, [{active, once}]),
    {noreply, State};

handle_info({tcp_closed, Socket}, #state{socket = Socket} = State) ->
    ?LOG_DEBUG("~p", [{tcp_closed, Socket}]),
    {stop, normal, State};

handle_info(_Unknown, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

%% =============================================================================
%% Internal functions
%% =============================================================================

do_send_message(Ref, <<_, _/binary>> = Msg) ->
    gen_server:call(Ref, {send_message, Msg});
do_send_message(_, _) ->
    {error, empty_message}.

connect_and_start(<<_, _/binary>> = Username, <<_, _/binary>> = Password, Opts) ->
    Port = maps:get(port, Opts, ?DEFAULT_PORT),
    case connect_and_login(Username, Password, Port) of
        {ok, Socket} ->
            {ok, Pid} = start_link(Opts#{socket => Socket, username => Username}),
            gen_tcp:controlling_process(Socket, Pid),
            {ok, Pid};
        {error, Reason} ->
            {error, Reason}
    end;
connect_and_start(_, _, _) ->
    {error, badarg}.

connect_and_login(Username, Password, Port) ->
    case gen_tcp:connect("localhost", Port, [binary, {packet, 2}, {active, false}], ?CONNECTION_TIMEOUT) of
        {ok, Socket} ->
            ?LOG_DEBUG("connected to server", []),
            case do_login(Username, Password, Socket) of
                ok ->
                    ?LOG_DEBUG("logged in", []),
                    {ok, Socket};
                {error, Reason} ->
                    ?LOG_DEBUG("login failed: ~p", [Reason]),
                    gen_tcp:close(Socket),
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

do_login(Username, Password, Socket) ->
    maybe
        ok ?= gen_tcp:send(Socket, make_login_message(Username, Password)),
        {ok, Reply} ?= gen_tcp:recv(Socket, 0, ?LOGIN_TIMEOUT),
        ok ?= parse_login_reply(Reply)
    else
        {error, Reason} ->
            {error, Reason}
    end.

make_login_message(Username, Password) ->
    Credentials = <<"username=", Username/binary, ",password=", Password/binary>>,
    <<"LOGIN", Credentials/binary>>.

make_message(<<_, _/binary>> = Msg) ->
    <<"MSG", Msg/binary>>.

to_bin(Atom) when is_atom(Atom) ->
    atom_to_binary(Atom);
to_bin(List) when is_list(List) ->
    list_to_binary(List);
to_bin(Binary) when is_binary(Binary) ->
    Binary.

maybe_send_event(EventData, #state{event_handler = EH, username = Username}) when is_pid(EH) ->
    Event = #{pid => self(), user => Username, data => EventData},
    EH ! {client_event, Event};
maybe_send_event(_, _) ->
    ok.

parse_login_reply(<<"LOGIN_REPLY", ?REPLY_SUCCESS/integer>>) ->
    ok;
parse_login_reply(<<"LOGIN_REPLY", Reply/integer>>) ->
    {error, reply_to_error(Reply)}.

reply_to_error(?REPLY_ALREADY_CONNECTED) ->
    already_connected;
reply_to_error(?REPLY_AUTH_ERROR) ->
    auth_error;
reply_to_error(_) ->
    other_error.

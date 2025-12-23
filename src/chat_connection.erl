-module(chat_connection).
-behaviour(gen_server).

-include("chat.hrl").
-include_lib("kernel/include/logger.hrl").

%% API
-export([start_link/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-record(state, {
    socket,
    username,
    logged = false,
    event_handler
}).

-define(PG_GROUP, chat_users).

%% =============================================================================
%% API
%% =============================================================================

-spec start_link(gen_tcp:socket(), map()) -> {ok, pid()} | {error, term()}.
start_link(Socket, Opts) ->
    gen_server:start_link(?MODULE, [Socket, Opts], []).

%% =============================================================================
%% gen_server callbacks
%% =============================================================================

init([Socket, Opts]) ->
    EventHandler = maps:get(event_handler, Opts, undefined),
    inet:setopts(Socket, [{active, once}]),
    {ok, #state{socket = Socket, event_handler = EventHandler}}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({tcp, Socket, <<"LOGIN", _/binary>>}, #state{socket = Socket, logged = true} = State) ->
    %% Повторный LOGIN — игнорируем
    inet:setopts(Socket, [{active, once}]),
    {noreply, State};

handle_info({tcp, Socket, <<"LOGIN", Data/binary>>}, #state{socket = Socket, logged = false} = State) ->
    Credentials = [list_to_tuple(binary:split(Part, <<"=">>)) || Part <- binary:split(Data, <<",">>)],
    Username = proplists:get_value(<<"username">>, Credentials),
    Password = proplists:get_value(<<"password">>, Credentials),
    case chat_server:login(Username, Password) of
        ok ->
            ?LOG_INFO("user ~p logged in", [Username]),
            pg:join(?PG_GROUP, self()),
            gen_tcp:send(Socket, <<"LOGIN_REPLY", ?REPLY_SUCCESS/integer>>),
            inet:setopts(Socket, [{active, once}]),
            {noreply, State#state{username = Username, logged = true}};
        {error, Error} ->
            ?LOG_INFO("user ~p login failed: ~p", [Username, Error]),
            Reply = error_to_reply(Error),
            gen_tcp:send(Socket, <<"LOGIN_REPLY", Reply/integer>>),
            {stop, normal, State}
    end;

handle_info({tcp, Socket, <<"MSG", M/binary>>}, #state{socket = Socket, username = Username, logged = true} = State) 
        when byte_size(M) > 0 ->
    ?LOG_DEBUG("msg from ~p: ~p", [Username, M]),
    maybe_send_event({msg, Username, M}, State),
    broadcast(M, Username),
    inet:setopts(Socket, [{active, once}]),
    {noreply, State};

handle_info({broadcast, FromUser, Text}, #state{socket = Socket} = State) ->
    Msg = <<"MSG", FromUser/binary, " : ", Text/binary>>,
    gen_tcp:send(Socket, Msg),
    {noreply, State};

handle_info({tcp, Socket, _Message}, #state{socket = Socket} = State) ->
    {stop, normal, State};

handle_info({tcp_closed, Socket}, #state{socket = Socket, username = Username} = State) ->
    ?LOG_INFO("connection closed for ~p", [Username]),
    maybe_send_event({tcp_closed, Socket}, State),
    {stop, normal, State};

handle_info({tcp_error, Socket, Reason}, #state{socket = Socket, username = Username} = State) ->
    ?LOG_INFO("connection error for ~p: ~p", [Username, Reason]),
    maybe_send_event({tcp_error, Socket, Reason}, State),
    {stop, {error, Reason}, State};

handle_info(Unknown, State) ->
    ?LOG_WARNING("unexpected message: ~p", [Unknown]),
    {noreply, State}.

terminate(_Reason, #state{}) ->
    ok.

%% =============================================================================
%% Internal functions
%% =============================================================================

broadcast(Text, FromUser) ->
    Self = self(),
    Members = pg:get_members(?PG_GROUP),
    [Pid ! {broadcast, FromUser, Text} || Pid <- Members, Pid =/= Self],
    ok.

error_to_reply(auth_error) ->
    ?REPLY_AUTH_ERROR;
error_to_reply(already_connected) ->
    ?REPLY_ALREADY_CONNECTED.

maybe_send_event(EventData, #state{event_handler = EH, username = Username}) when is_pid(EH) ->
    Event = #{pid => self(), user => Username, data => EventData},
    EH ! {connection_event, Event};
maybe_send_event(_, _) ->
    ok.


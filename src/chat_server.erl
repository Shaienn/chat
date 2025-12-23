-module(chat_server).
-behaviour(gen_server).

-include("chat.hrl").
-include_lib("kernel/include/logger.hrl").

%% API
-export([start/0, start/1, start_link/1]).
-export([register/2]).
-export([login/2]).
-export([info/0]).
-export([create_tables/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-record(state, {}).

-define(SESSIONS_TABLE, chat_sessions).

%% =============================================================================
%% API
%% =============================================================================

-spec start() -> {ok, pid()} | {error, term()}.
start() ->
    start(#{}).

-spec start(server_opts()) -> {ok, pid()} | {error, term()}.
start(Opts) ->
    start_link(Opts).

-spec start_link(server_opts()) -> {ok, pid()} | {error, term()}.
start_link(Opts) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Opts], []).

-spec register(binary() | atom() | string(), binary() | atom() | string()) -> ok | {error, badarg}.
register(Username, Password) ->
    do_register(to_bin(Username), to_bin(Password)).

-spec login(binary(), binary()) -> ok | {error, auth_error | already_connected}.
login(Username, Password) ->
    gen_server:call(?MODULE, {login, Username, Password, self()}).

-spec info() -> server_info().
info() ->
    gen_server:call(?MODULE, info).

-spec create_tables() -> ok.
create_tables() ->
    ets:new(?MODULE, [named_table, public, set]),
    ets:new(?SESSIONS_TABLE, [named_table, public, set]),
    ok.

%% =============================================================================
%% gen_server callbacks
%% =============================================================================

init([_Opts]) ->
    restore_sessions(),
    {ok, #state{}}.

handle_call({register, Username, Password}, _From, State) ->
    ets:insert(?MODULE, {Username, Password}),
    {reply, ok, State};

handle_call({login, Username, Password, ConnectionPid}, _From, State) ->
    case do_login(Username, Password) of
        ok ->
            monitor(process, ConnectionPid, [{tag, {user_down, Username}}]),
            ets:insert(?SESSIONS_TABLE, {Username, ConnectionPid}),
            {reply, ok, State};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

% Только для тестирования
handle_call(info, _From, State) ->
    AuthDB = maps:from_list(ets:tab2list(?MODULE)),
    Users = maps:from_list(ets:tab2list(?SESSIONS_TABLE)),
    Info = #{auth_db => AuthDB, users => Users},
    {reply, Info, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({{user_down, Username}, _Ref, process, _Pid, _Reason}, State) ->
    ets:delete(?SESSIONS_TABLE, Username),
    {noreply, State};

handle_info(Unknown, State) ->
    ?LOG_WARNING("unexpected message: ~p", [Unknown]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

%% =============================================================================
%% Internal functions
%% =============================================================================

do_register(<<_,_/binary>> = Username, <<_,_/binary>> = Password) ->
    case has_forbidden_chars(Username) orelse has_forbidden_chars(Password) of
        true -> {error, badarg};
        false -> gen_server:call(?MODULE, {register, Username, Password})
    end;
do_register(_, _) ->
    {error, badarg}.

has_forbidden_chars(Bin) ->
    binary:match(Bin, [<<"=">>, <<",">>]) =/= nomatch.

do_login(<<_,_/binary>> = Username, <<_,_/binary>> = Password) ->
    case ets:lookup(?MODULE, Username) of
        [{_, StoredPassword}] when Password =:= StoredPassword ->
            case ets:lookup(?SESSIONS_TABLE, Username) of
                [] -> ok;
                _ -> {error, already_connected}
            end;
        _ ->
            {error, auth_error}
    end;
do_login(_, _) ->
    {error, auth_error}.

to_bin(Atom) when is_atom(Atom) ->
    atom_to_binary(Atom);
to_bin(List) when is_list(List) ->
    list_to_binary(List);
to_bin(Binary) when is_binary(Binary) ->
    Binary.

restore_sessions() ->
    ets:foldl(fun({Username, Pid}, ok) ->
        case is_process_alive(Pid) of
            true ->
                monitor(process, Pid, [{tag, {user_down, Username}}]);
            false ->
                ets:delete(?SESSIONS_TABLE, Username)
        end,
        ok
    end, ok, ?SESSIONS_TABLE).

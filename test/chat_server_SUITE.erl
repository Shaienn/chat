-module(chat_server_SUITE).
-include_lib("common_test/include/ct.hrl").

%% CT callbacks
-export([
    all/0,
    groups/0,
    init_per_testcase/2,
    end_per_testcase/2
]).

%% Test cases
-export([
    register_valid_user/1,
    register_empty_username/1,
    register_forbidden_chars/1,
    register_duplicate_user/1,
    login_invalid_user/1,
    login_valid_user/1,
    login_valid_user_twice/1,
    message_from_client/1,
    two_clients_chat/1,
    three_clients_one_crash/1,
    server_crash/1,
    three_clients_one_handler_crash/1
]).

%% =============================================================================
%% CT Callbacks
%% =============================================================================

all() ->
    [
        {group, registration},
        {group, single_client},
        {group, multiple_clients}
    ].

groups() ->
    [
        {registration, [], [
            register_valid_user,
            register_empty_username,
            register_forbidden_chars,
            register_duplicate_user
        ]},
        {single_client, [], [
            login_invalid_user,
            login_valid_user,
            login_valid_user_twice,
            message_from_client,
            server_crash
        ]},
        {multiple_clients, [], [
            two_clients_chat,
            three_clients_one_crash,
            three_clients_one_handler_crash
        ]}
    ].

init_per_testcase(_TestCase, Config) ->
    {ok, SupPid} = chat_sup:start_link(#{event_handler => self()}),
    unlink(SupPid),
    [{sup_pid, SupPid} | Config].

end_per_testcase(_TestCase, Config) ->
    SupPid = ?config(sup_pid, Config),
    exit(SupPid, shutdown),
    ok.

%% =============================================================================
%% Registration Tests
%% =============================================================================

register_valid_user(_Config) ->
    U = <<"user1">>,
    P = <<"password123">>,
    ok = chat_server:register(U, P),
    #{auth_db := AuthDB} = chat_server:info(),
    #{U := P} = AuthDB.

register_empty_username(_Config) ->
    {error, badarg} = chat_server:register(<<>>, <<"password">>).

register_forbidden_chars(_Config) ->
    {error, badarg} = chat_server:register(<<"user=1">>, <<"password">>),
    {error, badarg} = chat_server:register(<<"user,1">>, <<"password">>),
    {error, badarg} = chat_server:register(<<"user1">>, <<"pass=word">>),
    {error, badarg} = chat_server:register(<<"user1">>, <<"pass,word">>).

register_duplicate_user(_Config) ->
    ok = chat_server:register(<<"user1">>, <<"password">>),
    ok = chat_server:register(<<"user1">>, <<"newpassword">>),
    #{auth_db := AuthDB} = chat_server:info(),
    #{<<"user1">> := <<"newpassword">>} = AuthDB.

%% =============================================================================
%% Client Tests
%% =============================================================================

login_invalid_user(_Config) ->
    {error, auth_error} = chat_client:start("user", "password", #{test_env => true}),
    {error, badarg} = chat_client:start("", "password", #{test_env => true}),
    {error, badarg} = chat_client:start("user", "", #{test_env => true}).

login_valid_user(_Config) ->
    ok = chat_server:register(<<"user1">>, <<"password123">>),
    {ok, ClientPid} = chat_client:start("user1", "password123", #{test_env => true}),
    gen_server:stop(ClientPid).

login_valid_user_twice(_Config) ->
    ok = chat_server:register(<<"user1">>, <<"password123">>),
    {ok, ClientPid} = chat_client:start("user1", "password123", #{test_env => true}),
    {error, already_connected} = chat_client:start("user1", "password123", #{test_env => true}),
    gen_server:stop(ClientPid).

message_from_client(_Config) ->
    ok = chat_server:register(<<"user1">>, <<"password123">>),
    {ok, ClientPid} = chat_client:start("user1", "password123", #{test_env => true, event_handler => self()}),
    ok = chat_client:send_message(ClientPid, "test_message1"),

    ensure_server_got_message(<<"user1">>, <<"test_message1">>),

    % Убедимся, что клиент не получает свои собственные сообщения.
    receive
        {client_event, #{pid := ClientPid, data := {got_message, _}}} ->
            ct:fail("client received own message")
    after 500 ->
        ok
    end,

    ok = chat_client:send_message(ClientPid, "test_message2"),
    ensure_server_got_message(<<"user1">>, <<"test_message2">>),

    gen_server:stop(ClientPid).

two_clients_chat(_Config) ->
    ok = chat_server:register(<<"user1">>, <<"password123">>),
    ok = chat_server:register(<<"user2">>, <<"password234">>),

    {ok, ClientPid1} = chat_client:start("user1", "password123", #{test_env => true, event_handler => self()}),
    {ok, ClientPid2} = chat_client:start("user2", "password234", #{test_env => true, event_handler => self()}),

    ok = chat_client:send_message(ClientPid1, "test_message1"),

    ensure_client_sent_message(ClientPid1, <<"test_message1">>),
    ensure_client_got_message(ClientPid2, <<"user1 : test_message1">>),

    ok = chat_client:send_message(ClientPid2, "test_message2"),

    ensure_client_sent_message(ClientPid2, <<"test_message2">>),
    ensure_client_got_message(ClientPid1, <<"user2 : test_message2">>),

    gen_server:stop(ClientPid1),
    gen_server:stop(ClientPid2).

three_clients_one_crash(_Config) ->
    ok = chat_server:register(<<"user1">>, <<"password123">>),
    ok = chat_server:register(<<"user2">>, <<"password234">>),
    ok = chat_server:register(<<"user3">>, <<"password345">>),

    {ok, ClientPid1} = chat_client:start("user1", "password123", #{test_env => true, event_handler => self()}),
    {ok, ClientPid2} = chat_client:start("user2", "password234", #{test_env => true, event_handler => self()}),
    {ok, ClientPid3} = chat_client:start("user3", "password345", #{test_env => true, event_handler => self()}),

    ok = chat_client:send_message(ClientPid1, "test_message1"),

    ensure_client_sent_message(ClientPid1, <<"test_message1">>),
    ensure_client_got_message(ClientPid2, <<"user1 : test_message1">>),
    ensure_client_got_message(ClientPid3, <<"user1 : test_message1">>),

    unlink(ClientPid2),
    exit(ClientPid2, test_crash),

    ok = chat_client:send_message(ClientPid1, "test_message2"),
    ensure_client_sent_message(ClientPid1, <<"test_message2">>),
    ensure_client_got_message(ClientPid3, <<"user1 : test_message2">>),

    gen_server:stop(ClientPid1),
    gen_server:stop(ClientPid3).


three_clients_one_handler_crash(_Config) ->
    ok = chat_server:register(<<"user1">>, <<"password123">>),
    ok = chat_server:register(<<"user2">>, <<"password234">>),
    ok = chat_server:register(<<"user3">>, <<"password345">>),

    {ok, ClientPid1} = chat_client:start("user1", "password123", #{test_env => true, event_handler => self()}),
    {ok, ClientPid2} = chat_client:start("user2", "password234", #{test_env => true, event_handler => self()}),
    {ok, ClientPid3} = chat_client:start("user3", "password345", #{test_env => true, event_handler => self()}),
    
    ok = chat_client:send_message(ClientPid1, "test_message1"),
    ensure_client_sent_message(ClientPid1, <<"test_message1">>),
    ensure_client_got_message(ClientPid2, <<"user1 : test_message1">>),
    ensure_client_got_message(ClientPid3, <<"user1 : test_message1">>),

    %% Получаем PID chat_connection для user2
    #{users := Users} = chat_server:info(),
    ConnectionPid2 = maps:get(<<"user2">>, Users),

    exit(ConnectionPid2, test_crash),

    ok = chat_client:send_message(ClientPid1, "test_message2"),
    ensure_client_sent_message(ClientPid1, <<"test_message2">>),
    ensure_client_got_message(ClientPid3, <<"user1 : test_message2">>),

    #{users := Users2} = chat_server:info(),
    false = maps:is_key(<<"user2">>, Users2),

    gen_server:stop(ClientPid1),
    gen_server:stop(ClientPid3).

server_crash(_Config) ->
    ok = chat_server:register(<<"user1">>, <<"password123">>),
    {ok, _} = chat_client:start("user1", "password123", #{test_env => true, event_handler => self()}),
    
    ServerPid = whereis(chat_server),
    MonRef = monitor(process, ServerPid),
    exit(ServerPid, test_crash),
    
    receive
        {'DOWN', MonRef, process, ServerPid, _Reason} -> ok
    after 1000 ->
        error(server_not_crashed)
    end,
    
    supervisor:which_children(chat_sup),
    NewServerPid = whereis(chat_server),
    true = is_pid(NewServerPid) andalso NewServerPid =/= ServerPid,
    
    %% Проверяем, что auth_db сохранилась и сессии восстановились
    #{auth_db := AuthDB, users := Users} = chat_server:info(),
    true = maps:is_key(<<"user1">>, AuthDB),
    true = maps:is_key(<<"user1">>, Users),
    ok.

%% =============================================================================
%% Helpers
%% =============================================================================

ensure_server_got_message(Username, Message) ->
    receive
        {connection_event, #{user := Username, data := {msg, Username, Message}}} -> ok
    after 1000 ->
        error(no_message)
    end.

ensure_client_sent_message(ClientPid, Message) ->
    receive
        {client_event, #{pid := ClientPid, data := {sent_message, Message}}} -> ok
    after 1000 ->
        error(no_message)
    end.

ensure_client_got_message(ClientPid, Message) ->
    receive
        {client_event, #{pid := ClientPid, data := {got_message, Message}}} -> ok
    after 1000 ->
        error(no_message)
    end.

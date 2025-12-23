-ifndef(CHAT_HRL).
-define(CHAT_HRL, true).

-type username() :: binary().
-type password() :: binary().
-type server_opts() :: #{
    port => inet:port_number(),
    event_handler => pid()
}.
-type client_opts() :: #{
    port => inet:port_number(),
    event_handler => pid(),
    test_env => boolean(),
    _ => _
}.
-type server_info() :: #{
    auth_db := #{username() => password()},
    users := #{username() => pid()}
}.

-export_type([username/0, password/0, server_opts/0, client_opts/0, server_info/0]).

-define(DEFAULT_PORT, 10000).
-define(CONNECTION_TIMEOUT, 5000).
-define(LOGIN_TIMEOUT, 5000).

-define(REPLY_SUCCESS, 0).
-define(REPLY_AUTH_ERROR, 1).
-define(REPLY_ALREADY_CONNECTED, 2).

-endif.

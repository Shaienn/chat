# Erlang Chat Server

Консольный чат на Erlang OTP: сервер + клиент.

## Требования

- Erlang/OTP 27 - ребар скомпилирован под эту версию

Запускал в `erlang:27` контейнере

## Сборка

```bash
./rebar3 compile
```

## Запуск сервера

```bash
./run_server.sh
```

### Добавление пользователей (в консоли сервера)

```erlang
chat_server:register("alice", "password123").
chat_server:register("bob", "secret").
```

### Просмотр состояния

```erlang
chat_server:info().
%% #{auth_db => #{<<"alice">> => <<"password123">>}, users => #{}}
```

## Запуск клиента

В отдельном терминале:

```bash
./run_client.sh
```

### Подключение и отправка сообщений (в консоли клиента)

```erlang
%% Подключиться (имя, пароль)
{ok, _} = chat_client:start("alice", "password123").

%% Отправить сообщение
chat_client:send_message("Hello everyone!").
```

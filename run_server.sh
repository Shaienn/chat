#!/bin/bash

./rebar3 compile

exec erl \
    -pa _build/default/lib/*/ebin \
    -config config/sys \
    -sname chat_server \
    -s chat_server_app

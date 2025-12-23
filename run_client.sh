#!/bin/bash

./rebar3 compile

exec erl \
    -pa _build/default/lib/*/ebin \
    -config config/sys \
    -sname "client_$$"

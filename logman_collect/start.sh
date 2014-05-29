#!/bin/bash

erl -pa ebin deps/erlzmq/ebin deps/msgpack/ebin deps/emysql/ebin -config config -K true -eval "application:start(sasl),application:start(logman)." -noshell -detached


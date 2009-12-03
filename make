#!/usr/bin/env bash

erlc -pa . *erl
erlc *test.erl
erl -s gerl_test test -noshell -s init stop

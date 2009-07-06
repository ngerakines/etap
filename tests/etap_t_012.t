#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa ../src -sasl errlog_type error -boot start_sasl

main(_) ->
    etap_t_012:start().

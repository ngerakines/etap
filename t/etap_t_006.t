#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa ./ebin -pa ./t

main(_) ->
    etap:plan(2),
    etap:skip(fun() ->
        etap:ok(false, "This shouldn't be false ...")
    end),
    etap:skip(fun() ->
        etap:ok(false, "This shouldn't be false ...")
    end, "Fails for reasons unknown"),
    etap:end_tests().

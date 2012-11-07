#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa ./ebin -pa ./t

main(_) ->
    etap:plan(unknown),
    etap:is(1, 1, "one down"),
    etap:is(3, 3, "two down"),
    etap:is(4, 4, "three down"),
    etap:is(5, 5, "four down"),
    etap:end_tests(),
    ok.

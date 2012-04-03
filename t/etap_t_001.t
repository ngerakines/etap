#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa ./ebin -pa ./t

main(_) ->
    etap:plan(45),
    etap:diag("Test the etap module."),
    test_etap(),
    test_etap_application(),
    test_etap2(),
    test_etap_exceptions(),
    test_etap_process(),
    etap:end_tests().

test_etap() ->
    etap:loaded_ok(etap, "module 'etap' loaded"),
    etap:can_ok(etap, plan),
    etap:can_ok(etap, plan, 1),
    etap:can_ok(etap, end_tests),
    etap:can_ok(etap, end_tests, 0),
    etap:can_ok(etap, diag),
    etap:can_ok(etap, diag, 1),
    etap:can_ok(etap, ok),
    etap:can_ok(etap, ok, 2),
    etap:can_ok(etap, not_ok),
    etap:can_ok(etap, not_ok, 2),
    etap:can_ok(etap, is),
    etap:can_ok(etap, is, 3),
    etap:can_ok(etap, is_greater),
    etap:can_ok(etap, is_greater, 3),
    etap:can_ok(etap, isnt),
    etap:can_ok(etap, isnt, 3),
    etap:can_ok(etap, any),
    etap:can_ok(etap, any, 3),
    etap:can_ok(etap, none),
    etap:can_ok(etap, none, 3),
    etap:can_ok(etap, fun_is),
    etap:can_ok(etap, fun_is, 3),
    ok.

test_etap_application() ->
    etap:can_ok(etap, loaded_ok),
    etap:can_ok(etap, loaded_ok, 2),
    ok.

test_etap2() ->
    etap:can_ok(etap, loaded_ok),
    etap:can_ok(etap, loaded_ok, 2),
    etap:can_ok(etap, can_ok),
    etap:can_ok(etap, can_ok, 2),
    etap:can_ok(etap, can_ok, 3),
    etap:can_ok(etap, has_attrib, 2),
    etap:can_ok(etap, is_attrib, 3),
    etap:can_ok(etap, is_behaviour, 2),
    ok.

test_etap_exceptions() ->
    etap:can_ok(etap, dies_ok),
    etap:can_ok(etap, dies_ok, 2),
    etap:can_ok(etap, lives_ok),
    etap:can_ok(etap, lives_ok, 2),
    etap:can_ok(etap, throws_ok),
    etap:can_ok(etap, throws_ok, 3),
    ok.


test_etap_process() ->
    etap:can_ok(etap, is_pid),
    etap:can_ok(etap, is_pid, 2),
    etap:can_ok(etap, is_alive),
    etap:can_ok(etap, is_alive, 2),
    etap:can_ok(etap, is_mfa),
    etap:can_ok(etap, is_mfa, 3),
    ok.

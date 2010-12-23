-module(etap_t_002).
-export([start/0]).

start() ->
    etap:plan(67),
    etap:diag("Test the can_ok."),
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
    etap:can_ok(etap, start_ok),
    etap:can_ok(etap, start_ok, 2),
    etap:can_ok(etap, ensure_loaded),
    etap:can_ok(etap, ensure_loaded, 3),
    etap:can_ok(etap, pg2_group_exists),
    etap:can_ok(etap, pg2_group_exists, 2),
    etap:can_ok(etap, pg2_group_exists),
    etap:can_ok(etap, pg2_group_exists, 2),
    etap:can_ok(etap, load_ok),
    etap:can_ok(etap, load_ok, 2),
    etap:can_ok(etap, loaded_ok),
    etap:can_ok(etap, loaded_ok, 2),
    etap:can_ok(etap, can_ok),
    etap:can_ok(etap, can_ok, 2),
    etap:can_ok(etap, can_ok, 3),
    etap:can_ok(etap, has_attrib, 2),
    etap:can_ok(etap, is_attrib, 3),
    etap:can_ok(etap, is_behaviour, 2),
    etap:can_ok(etap, dies_ok),
    etap:can_ok(etap, dies_ok, 2),
    etap:can_ok(etap, lives_ok),
    etap:can_ok(etap, lives_ok, 2),
    etap:can_ok(etap, throws_ok),
    etap:can_ok(etap, throws_ok, 3),
    etap:can_ok(etap, status_is),
    etap:can_ok(etap, status_is, 3),
    etap:can_ok(etap, simple_200),
    etap:can_ok(etap, simple_200, 2),
    etap:can_ok(etap, simple_404),
    etap:can_ok(etap, simple_404, 2),
    etap:can_ok(etap, build_request),
    etap:can_ok(etap, build_request, 4),
    etap:can_ok(etap, is_pid),
    etap:can_ok(etap, is_pid, 2),
    etap:can_ok(etap, is_alive),
    etap:can_ok(etap, is_alive, 2),
    etap:can_ok(etap, is_mfa),
    etap:can_ok(etap, is_mfa, 3),
    etap:end_tests().

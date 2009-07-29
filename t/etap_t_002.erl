-module(etap_t_002).
-export([start/0]).

start() ->
    etap:plan(67),
    etap:diag("Test the etap_can module."),
    test_etap(),
    test_etap_application(),
    test_etap_can(),
    test_etap_exception(),
    test_etap_request(),
    test_etap_web(),
    test_etap_process(),
    etap:end_tests().

test_etap() ->
    etap_can:loaded_ok(etap, "module 'etap' loaded"),
    etap_can:can_ok(etap, plan),
    etap_can:can_ok(etap, plan, 1),
    etap_can:can_ok(etap, end_tests),
    etap_can:can_ok(etap, end_tests, 0),
    etap_can:can_ok(etap, diag),
    etap_can:can_ok(etap, diag, 1),
    etap_can:can_ok(etap, ok),
    etap_can:can_ok(etap, ok, 2),
    etap_can:can_ok(etap, not_ok),
    etap_can:can_ok(etap, not_ok, 2),
    etap_can:can_ok(etap, is),
    etap_can:can_ok(etap, is, 3),
    etap_can:can_ok(etap, is_greater),
    etap_can:can_ok(etap, is_greater, 3),
    etap_can:can_ok(etap, isnt),
    etap_can:can_ok(etap, isnt, 3),
    etap_can:can_ok(etap, any),
    etap_can:can_ok(etap, any, 3),
    etap_can:can_ok(etap, none),
    etap_can:can_ok(etap, none, 3),
    etap_can:can_ok(etap, fun_is),
    etap_can:can_ok(etap, fun_is, 3),
    ok.

test_etap_application() ->
    etap_can:loaded_ok(etap_application, "module 'etap_application' loaded"),
    etap_can:can_ok(etap_application, start_ok),
    etap_can:can_ok(etap_application, start_ok, 2),
    etap_can:can_ok(etap_application, ensure_loaded),
    etap_can:can_ok(etap_application, ensure_loaded, 3),
    etap_can:can_ok(etap_application, pg2_group_exists),
    etap_can:can_ok(etap_application, pg2_group_exists, 2),
    etap_can:can_ok(etap_application, pg2_group_exists),
    etap_can:can_ok(etap_application, pg2_group_exists, 2),
    etap_can:can_ok(etap_application, load_ok),
    etap_can:can_ok(etap_application, load_ok, 2),
    ok.

test_etap_can() ->
    etap_can:loaded_ok(etap_can, "module 'etap_can' loaded"),
    etap_can:can_ok(etap_can, loaded_ok),
    etap_can:can_ok(etap_can, loaded_ok, 2),
    etap_can:can_ok(etap_can, can_ok),
    etap_can:can_ok(etap_can, can_ok, 2),
    etap_can:can_ok(etap_can, can_ok, 3),
    etap_can:can_ok(etap_can, has_attrib, 2),
    etap_can:can_ok(etap_can, is_attrib, 3),
    etap_can:can_ok(etap_can, is_behaviour, 2),
    ok.

test_etap_exception() ->
    etap_can:loaded_ok(etap_exception, "module 'etap_exception' loaded"),
    etap_can:can_ok(etap_exception, dies_ok),
    etap_can:can_ok(etap_exception, dies_ok, 2),
    etap_can:can_ok(etap_exception, lives_ok),
    etap_can:can_ok(etap_exception, lives_ok, 2),
    etap_can:can_ok(etap_exception, throws_ok),
    etap_can:can_ok(etap_exception, throws_ok, 3),
    ok.

test_etap_request() ->
    etap_can:loaded_ok(etap_request, "module 'etap_request' loaded"),
    etap_can:can_ok(etap_request, status_is),
    etap_can:can_ok(etap_request, status_is, 3),
    ok.

test_etap_web() ->
    etap_can:loaded_ok(etap_web, "module 'etap_web' loaded"),
    etap_can:can_ok(etap_web, simple_200),
    etap_can:can_ok(etap_web, simple_200, 2),
    etap_can:can_ok(etap_web, simple_404),
    etap_can:can_ok(etap_web, simple_404, 2),
    etap_can:can_ok(etap_web, build_request),
    etap_can:can_ok(etap_web, build_request, 4),
    ok.

test_etap_process() ->
    etap_can:loaded_ok(etap_process, "module 'etap_process' loaded"),
    etap_can:can_ok(etap_process, is_pid),
    etap_can:can_ok(etap_process, is_pid, 2),
    etap_can:can_ok(etap_process, is_alive),
    etap_can:can_ok(etap_process, is_alive, 2),
    etap_can:can_ok(etap_process, is_mfa),
    etap_can:can_ok(etap_process, is_mfa, 3),
    ok.

-module(etap_can_t).
-export([start/0]).

-import(etap, [plan/1, ok/2, not_ok/2,
        end_tests/0]).
-import(etap_can, [loaded_ok/2, test_loaded/1, try_load/1,
        can_ok/2, can_ok/3, function_exists/2, function_exists/3]).

start() ->
    plan(8),
    loaded_ok(etap, "Etap module is loaded"),
    ok(test_loaded(etap), "etap test_loaded returns true"),
    not_ok(test_loaded(etap_exception), "etap_exception test_loaded returns false"),
    ok(try_load(etap_exception), "etap_exception try_loaded returns true"),
    can_ok(etap, ok),
    not_ok(function_exists(etap, ok1), "ok1 does not exist"),
    can_ok(etap_can, can_ok, 3),
    not_ok(function_exists(etap, ok, 3), "ok/3 does not exist"),
    end_tests().

-module(etap_exception_t).
-export([start/0]).

-import(etap, [plan/1, end_tests/0]).
-import(etap_exception, [dies_ok/2, lives_ok/2]).

start() ->
    plan(1),
    dies_ok(fun () -> throw("some error") end, "throwing an error dies"),
    lives_ok(fun () -> M=1 end, "not throwing an error lives"),
    end_tests().

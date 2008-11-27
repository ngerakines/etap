-module(etap_t_003).
-export([start/0]).

start() ->
    etap:plan(2),
    etap_exception:dies_ok(fun() -> throw("some error") end, "throwing an error dies"),
    etap_exception:lives_ok(fun() -> M = 1 end, "not throwing an error lives"),
    etap_exception:throws_ok(fun() -> throw("error_foo") end, "error_foo", "throwing an error dies"),
    etap:end_tests().

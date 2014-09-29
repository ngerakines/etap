-module(etap_t_003).
-export([start/0]).

start() ->
    etap:plan(3),
    etap:dies_ok(fun() -> exit("some error") end, "throwing an error dies"),
    etap:lives_ok(fun() -> _M = 1 end, "not throwing an error lives"),
    etap:throws_ok(fun() -> throw("error_foo") end, "error_foo", "throwing an error dies"),
    etap:end_tests().

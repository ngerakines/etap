-module(etapis_t).

-export([start/0]).

-import(etap, [plan/1, isnt/3, is/3, end_tests/0]).

start() ->
    plan(2),
    is(1, 1, "1 is 1"),
    isnt(1, 2, "1 is not 2"),
    end_tests().

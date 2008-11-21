-module(etapok_t).

-export([start/0]).

-import(etap, [plan/1, ok/2, not_ok/2,end_tests/0]).

start() ->
    plan(2),
    ok(true, "true is ok"),
    not_ok(false, "false is not ok"),
    end_tests().

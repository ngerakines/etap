-module(etap_server_t).

-export([start/0]).

-import(etap, [ok/2, plan/1, end_tests/0]).

start() ->
    plan(1),
    ok(true, "true is ok"),
    etap:diag("you should see a message about the test server already started next"),
    etap:test_server(),
    end_tests().

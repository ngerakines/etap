-module (etap_t_007).

-export([start/0]).

start() ->
    etap:plan(5),
    etap:is(timer:sleep(500), ok, "Sleeping for 500ms"),
    etap:diag_time(),
    etap:is(timer:sleep(500), ok, "Sleeping for 500ms"),
    etap:diag_time(),
    etap:is(timer:sleep(500), ok, "Sleeping for 500ms"),
    etap:diag_time(),
    etap:is(timer:sleep(500), ok, "Sleeping for 500ms"),
    etap:diag_time(),
    etap:is(timer:sleep(500), ok, "Sleeping for 500ms"),
    etap:end_tests().

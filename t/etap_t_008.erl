-module (etap_t_008).

-export([start/0]).

start() ->
    etap:plan(3),
    etap:diag("probably stupid but I don't know what else to test this against"),
    etap:has_attrib(kernel, behaviour),
    etap:is_attrib(kernel, behaviour, supervisor),
    etap:is_behaviour(kernel, supervisor),
    etap:end_tests().

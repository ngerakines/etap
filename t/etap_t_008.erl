-module (etap_t_008).

-export([start/0]).

start() ->
    etap:plan(3),
    etap:diag("probably stupid but I don't know what else to test this against"),
    etap_can:has_attrib(kernel, behaviour),
    etap_can:is_attrib(kernel, behaviour, supervisor),
    etap_can:is_behaviour(kernel, supervisor),
    etap:end_tests().

-module(etap_t_011).

-export([start/0]).

start() ->
    etap:plan(skip),
    _Skipped = fun() ->
        etap:skip(fun() ->
            etap:ok(false, "This shouldn't be false ...")
        end),
        etap:skip(fun() ->
            etap:ok(false, "This shouldn't be false ...")
        end, "Fails for reasons unknown")
    end,
    etap:end_tests().

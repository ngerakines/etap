-module(etap_t_004).
-export([start/0, start_loop/1]).

start() ->
    etap:plan(6),
    etap:load_ok(inets, "Application inets-5.0.13 loads"),
    etap:start_ok(inets, "Application inets-5.0.13 starts"),
    etap:skip(fun() ->
        etap:ensure_loaded(inets, "5.1", "Application inets-5.1 is loaded and running.")
    end, "Versions may be different"),

    spawn(?MODULE, start_loop, [test_group_a]),
    timer:sleep(100),
    etap:pg2_group_exists(test_group_a, "pg2 group test_group_a exists"),
    etap:pg2_group_doesntexist(test_group_b, "pg2 group test_group_b exists"),

    spawn(?MODULE, start_loop, [test_group_b]),
    timer:sleep(100),
    etap:pg2_group_exists(test_group_b, "pg2 group test_group_b exists"),

    [ Pid ! done || Pid <- pg2:get_members(test_group_a)],
    [ Pid ! done || Pid <- pg2:get_members(test_group_b)],

    etap:end_tests().

start_loop(GroupName) ->
    pg2:create(GroupName),
    pg2:join(GroupName, self()),
    loop().

loop() ->
    receive
        done -> exit(normal);
        _ -> ok
    end,
    loop().

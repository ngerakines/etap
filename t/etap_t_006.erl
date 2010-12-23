-module(etap_t_006).
-export([start/0, start_loop/1]).

start() ->
    etap:plan(3),

    etap:is_pid(spawn(?MODULE, start_loop, [test_group_a]), "Spawned process is a pid"),
    etap:is_alive(spawn(?MODULE, start_loop, [test_group_a]), "Spawned process is alive"),
    etap:is_mfa(spawn(?MODULE, start_loop, [test_group_a]), {?MODULE, start_loop, 1}, "Spawned process has correct MFA"),
    [ Pid ! done || Pid <- pg2:get_members(test_group_a)],
    etap:end_tests().

start_loop(GroupName) ->
    pg2:create(GroupName),
    pg2:join(GroupName, self()),
    loop().

loop() ->
    timer:sleep(10000),
    receive done -> exit(normal); _ -> ok end,
    loop().

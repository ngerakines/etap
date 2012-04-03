#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa ./ebin -pa ./t

-module(etap_t_006).

-export([main/1, start_loop/1, loop/0]).

start_loop(GroupName) ->
    pg2:create(GroupName),
    pg2:join(GroupName, self()),
    loop().

loop() ->
    timer:sleep(10000),
    receive done -> exit(normal); _ -> ok end,
    loop().

main(_) ->
    etap:plan(3),

    etap:is_pid(spawn(?MODULE, start_loop, [test_group_a]), "Spawned process is a pid"),
    etap:is_alive(spawn(?MODULE, start_loop, [test_group_a]), "Spawned process is alive"),
    etap:is_mfa(spawn(?MODULE, start_loop, [test_group_a]), {?MODULE, start_loop, 1}, "Spawned process has correct MFA"),
    etap:end_tests().



%% @doc Adds process/pid testing to the etap suite.
-module(etap_process).

-export([is_pid/2, is_alive/2, is_mfa/3]).

% ---
% External / Public functions

%% @doc Assert that a given variable is a pid.
is_pid(Pid, Desc) ->
    etap:ok(erlang:is_pid(Pid), Desc).

%% @doc Assert that a given process/pid is alive.
is_alive(Pid, Desc) ->
    etap:ok(erlang:is_process_alive(Pid), Desc).

is_mfa(Pid, MFA, Desc) ->
    etap:is({current_function, MFA}, erlang:process_info(Pid, current_function), Desc).

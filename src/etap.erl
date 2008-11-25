%% ChangeLog
%% - 2008-11-25 ngerakines
%%   - Consolidated test server and plan server.
%%   - Added meta information when creating new plan.
%% @author Nick Gerakines <nick@gerakines.net> [http://socklabs.com/]
%% @author Jeremy Wall <jeremy@marzhillstudios.com
%% @version 0.3
%% @copyright 2007-2008 Jeremy Wall
%% @reference http://testanything.org/wiki/index.php/Main_Page
%% @reference http://en.wikipedia.org/wiki/Test_Anything_Protocol
%% @doc etap is a TAP testing module for Erlang components and applications.
%% This module allows developers to test their software using the TAP method.
%% 
%% <blockquote cite="http://en.wikipedia.org/wiki/Test_Anything_Protocol"><p>
%% TAP, the Test Anything Protocol, is a simple text-based interface between
%% testing modules in a test harness. TAP started life as part of the test
%% harness for Perl but now has implementations in C/C++, Python, PHP, Perl
%% and probably others by the time you read this.
%% </p></blockquote>
%% 
%% The testing process begins by defining a plan using etap:plan/1, running
%% a number of etap tests and then calling eta:end_tests/0. Please refer to
%% the Erlang modules in the t directory of this project for example tests.
-module(etap).
-export([
    ensure_test_server/0, start_etap_server/0, test_server/1,
    diag/1, plan/1,
    end_tests/0, not_ok/2, ok/2, is/3, isnt/3
]).

-record(test_state, {planned = 0, count = 0, pass = 0, fail = 0, skip = 0}).

%% @doc Create a test plan and boot strap the test server.
plan(N) when is_integer(N), N > 0 ->
    ensure_test_server(),
    etap_server ! {self(), plan, N},
    ok.

ensure_test_server() ->
    case whereis(etap_server) of
        undefined ->
            proc_lib:start(?MODULE, start_etap_server,[]);
        _ ->
            diag("The test server is already running.")
    end.

start_etap_server() ->
    catch register(etap_server, self()),
    proc_lib:init_ack(ok),
    etap:test_server(#test_state{}).

test_server(State) ->
    NewState = receive
        {_From, plan, N} ->
            io:format("1..~p~n", [N]),
            io:format("# Current time local ~s~n", [datetime(erlang:localtime())]),
            io:format("# Using etap version 0.3~n"),
            #test_state{
                planned = N, count = 0, pass = 0, fail = 0, skip = 0
            };
        {_From, pass, N} ->
            #test_state{
                count = State#test_state.count + N,
                pass = State#test_state.pass + N
            };
        {_From, fail, N} ->
            #test_state{
                count = State#test_state.count + N,
                fail = State#test_state.fail + N
            };
        {_From, skip, N} ->
            #test_state{
                count = State#test_state.count + N,
                skip = State#test_state.skip + N
            };
        {From, state} ->
            From ! State,
            State;
        {From, count} ->
            From ! State#test_state.count,
            State;
        done ->
            io:format("Ran ~p Tests Passed: ~p Failed: ~p Skipped: ~p~n~n", [State#test_state.count, State#test_state.pass, State#test_state.fail, State#test_state.skip]),
            exit(normal)
    end,
    test_server(NewState).

end_tests() -> etap_server ! done.

mk_tap(Result, Desc) ->
    N = lib:sendw(etap_server, count),
    case Result of
        true ->
            output(lists:concat(["ok ", N, " -  ",  Desc])),
            etap_server ! {self(), pass, 1};
            
        false ->
            output(lists:concat(["not ok ", N, " -  ",  Desc])),
            etap_server ! {self(), fail, 1}
    end.

output(S) -> io:format("~s~n", [S]).
    
diag(S) -> output("# "++S).

ok(Expr, Desc) -> mk_tap(Expr, Desc).

not_ok(Expr, Desc) -> mk_tap(not Expr, Desc).

is(Got, Expected, Desc) -> mk_tap(Got == Expected, Desc).

isnt(Got, Expected, Desc) -> mk_tap(Got /= Expected, Desc).

datetime(DateTime) ->
    {{Year, Month, Day}, {Hour, Min, Sec}} = DateTime,
    io_lib:format("~4.10.0B-~2.10.0B-~2.10.0B ~2.10.0B:~2.10.0B:~2.10.0B", [Year, Month, Day, Hour, Min, Sec]).

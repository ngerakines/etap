%% Copyright (c) 2008 Nick Gerakines <nick@gerakines.net>
%% 
%% Permission is hereby granted, free of charge, to any person
%% obtaining a copy of this software and associated documentation
%% files (the "Software"), to deal in the Software without
%% restriction, including without limitation the rights to use,
%% copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the
%% Software is furnished to do so, subject to the following
%% conditions:
%% 
%% The above copyright notice and this permission notice shall be
%% included in all copies or substantial portions of the Software.
%% 
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
%% EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
%% OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
%% NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
%% HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
%% WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
%% FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
%% OTHER DEALINGS IN THE SOFTWARE.
%%
%% ChangeLog
%% - 2008-12-30 ngerakines
%%   - Removing functionality, hurray!
%% - 2008-12-28 ngerakines
%%   - Added etap:is_greater/3
%%   - Added etap_string module and updated test suite
%% - 2008-12-26 ngerakines
%%   - Cleaned etap_web:build_request/4 module handling of redirects
%%   - Added functionality to etal_request
%% - 2008-12-17 Jacob Vorreuter, ngerakines, Jeremy Wall
%%   - Cleaned etap_web:build_request/4 module handling for get/post/put requests
%%   - Documentation updates / cleanup
%% - 2008-12-17 Jacob Vorreuter, ngerakines, Jeremy Wall
%%   - Fixing bug in etap_request:status_is/2
%%   - Added attribute inspection tests to etap_can
%%   - Added Jacob to credits list
%% - 2008-12-15 Jeremy Wall, ngerakines
%%   - Added doc targets to build
%%   - Misc build cleanup
%% - 2008-12-11 ngerakines
%%   - Added etap:diag_time/0
%% - 2008-12-10 ngerakines
%%   - Adding support for non-get requests in etap_web.
%% - 2008-12-09 ngerakines
%%   - Added output displaying test run time in seconds.
%% - 2008-12-01 ngerakines
%%   - Fixed bug in test etap_t_002.
%%   - Minor/Misc code cleanup.
%% - 2008-11-30 ngerakines
%%   - Fixed by in test results where failed tests weren't being recorded.
%%   - Added warning when planned vs executed tests aren't the same.
%%   - Bumping rev to 0.3.1.
%%   - Added etap:fun_is/3.
%%   - Updated the README
%%   - Added limited support for the dianostic syntax
%% - 2008-11-28 ngerakines
%%   - Minor documentation and build changes.
%%   - Added etap_process module and updated test suite accordingly.
%% - 2008-11-27 ngerakines
%%   - Added etap:any/3, etap:none/3 and etap_excecption:throws_ok/3.
%%   - Internal cleanup of etap modules.
%% - 2008-11-25 ngerakines
%%   - Consolidated test server and plan server.
%%   - Added meta information when creating new plan.
%%   - Added lots of documentation.
%%   - Cleaned up the current test suite.
%%   - Started extending testing capabilities of etap_request.
%% 
%% @author Nick Gerakines <nick@gerakines.net> [http://socklabs.com/]
%% @author Jeremy Wall <jeremy@marzhillstudios.com>
%% @version 0.3.1
%% @copyright 2007-2008 Jeremy Wall
%% @reference http://testanything.org/wiki/index.php/Main_Page
%% @reference http://en.wikipedia.org/wiki/Test_Anything_Protocol
%% @todo Finish implementing the skip directive.
%% @todo Document the messages handled by this receive loop.
%% @todo Explain in documentation why we use a process to handle test input.
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
    diag/1, plan/1, end_tests/0, not_ok/2, ok/2, is/3, isnt/3,
    any/3, none/3, fun_is/3, diag_time/0, is_greater/3
]).

-record(test_state, {planned = 0, count = 0, pass = 0, fail = 0, skip = 0, start_time}).

% ---
% External / Public functions

%% @doc Create a test plan and boot strap the test server.
plan(N) when is_integer(N), N > 0 ->
    ensure_test_server(),
    etap_server ! {self(), plan, N},
    ok.

%% @doc End the current test plan and output test results.
end_tests() -> etap_server ! done.

%% @doc Print a debug/status message related to the test suite.
diag(S) -> etap_server ! {self(), log, "# " ++ S}.

%% @doc Assert that a statement is true.
ok(Expr, Desc) -> mk_tap(Expr == true, Desc).

%% @doc Assert that a statement is false.
not_ok(Expr, Desc) -> mk_tap(Expr == false, Desc).

%% @doc Assert that two values are the same.
is(Got, Expected, Desc) ->
    case mk_tap(Got == Expected, Desc) of
        false ->
            etap_server ! {self(), log, "    ---"},
            etap_server ! {self(), log, io_lib:format("    description: ~p", [Desc])},
            etap_server ! {self(), log, io_lib:format("    found:       ~p", [Got])},
            etap_server ! {self(), log, io_lib:format("    wanted:      ~p", [Expected])},
            etap_server ! {self(), log, "    ..."},
            false;
        true -> true
    end.

%% @doc Assert that two values are not the same.
isnt(Got, Expected, Desc) -> mk_tap(Got /= Expected, Desc).

%% @doc Assert that an integer is greater than another.
is_greater(ValueA, ValueB, Desc) when is_integer(ValueA), is_integer(ValueB) ->
    mk_tap(ValueA > ValueB, Desc).

%% @doc Assert that an item is in a list.
any(Got, Items, Desc) ->
    is(lists:any(Got, Items), true, Desc).

%% @doc Assert that an item is not in a list.
none(Got, Items, Desc) ->
    is(lists:any(Got, Items), false, Desc).

%% @doc Use an anonymous function to assert a pattern match.
fun_is(Fun, Expected, Desc) when is_function(Fun) ->
    is(Fun(Expected), true, Desc).

diag_time() ->
    State = lib:sendw(etap_server, state),
    {Sm, Ss, Si} = State#test_state.start_time,
    {Em, Es, Ei} = erlang:now(),
    {Tm, Ts, Ti} = {Em - Sm, Es - Ss, Ei - Si},
    Message = io_lib:format("Time: ~B.~6..0B seconds.", [(Tm * 1000) + Ts, Ti]),
    diag(Message).

% ---
% Internal / Private functions

%% @private
%% @doc Start the etap_server process if it is not running already.
ensure_test_server() ->
    case whereis(etap_server) of
        undefined ->
            proc_lib:start(?MODULE, start_etap_server,[]);
        _ ->
            diag("The test server is already running.")
    end.

%% @private
%% @doc Start the etap_server loop and register itself as the etap_server
%% process.
start_etap_server() ->
    catch register(etap_server, self()),
    proc_lib:init_ack(ok),
    etap:test_server(#test_state{ planned = 0, count = 0, pass = 0, fail = 0, skip = 0 }).


%% @private
%% @doc The main etap_server receive/run loop. The etap_server receive loop
%% responds to seven messages apperatining to failure or passing of tests.
%% It is also used to initiate the testing process with the {_, plan, _}
%% message that clears the current test state.
test_server(State) ->
    NewState = receive
        {_From, plan, N} ->
            io:format("1..~p~n", [N]),
            io:format("# Current time local ~s~n", [datetime(erlang:localtime())]),
            io:format("# Using etap version 0.3~n"),
            State#test_state{
                planned = N, count = 0, pass = 0, fail = 0, skip = 0, start_time = erlang:now()
            };
        {_From, pass, N} ->
            State#test_state{
                count = State#test_state.count + N,
                pass = State#test_state.pass + N
            };
        {_From, fail, N} ->
            State#test_state{
                count = State#test_state.count + N,
                fail = State#test_state.fail + N
            };
        {_From, skip, N} ->
            State#test_state{
                count = State#test_state.count + N,
                skip = State#test_state.skip + N
            };
        {From, state} ->
            From ! State,
            State;
        {_From, log, Message} ->
            io:format("~s~n", [Message]),
            State;
        {From, count} ->
            From ! State#test_state.count,
            State;
        done ->
            exit(normal)
    end,
    test_server(NewState).

%% @private
%% @doc Process the result of a test and send it to the etap_server process.
mk_tap(Result, Desc) ->
    N = lib:sendw(etap_server, count),
    case Result of
        true ->
            etap_server ! {self(), log, lists:concat(["ok ", N + 1, " -  ",  Desc])},
            etap_server ! {self(), pass, 1},
            true;
        false ->
            etap_server ! {self(), log, lists:concat(["not ok ", N + 1,  " -  ",  Desc])},
            etap_server ! {self(), fail, 1},
            false
    end.

%% @private
%% @doc Format a date/time string.
datetime(DateTime) ->
    {{Year, Month, Day}, {Hour, Min, Sec}} = DateTime,
    io_lib:format("~4.10.0B-~2.10.0B-~2.10.0B ~2.10.0B:~2.10.0B:~2.10.0B", [Year, Month, Day, Hour, Min, Sec]).

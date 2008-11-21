-module(etap).
-export([
    plan_server/1, test_server/0, test_server/1, diag/1, plan/1,
    end_tests/0, not_ok/2, ok/2, is/3, isnt/3
]).

-vsn('0.2.1').
-description("A simple TAP compliant testing suite").

-record(test, {count=1, pass, fail}).

plan(N) ->
    %% is plan serve already registerd as a process?
    case erlang:whereis(plan_serv) of 
      undefined -> %% ok then it's time to start the plan_serve
          case is_number(N) of
              true ->
                  P = spawn(etap, plan_server, [N]),
                  register(plan_serv, P),
                  io:format("1..~p~n", [N]);
              false ->
                  throw("Not a number passed into plan")
          end;
      _ -> %% oops plan has already been started 
          throw("You have already made a plan!!")
    end,
    test_server().

plan_server(N) ->
    receive
        stop -> exit(normal);
        {Pid} -> Pid ! N
    end,
    plan_server(N).

%% the test server keeps track of the test progress
test_server() ->
    case lists:member(test_serv, registered()) of
      false ->
          register(test_serv, spawn(etap, test_server, [#test{count=1, pass=0, fail=0}]));
      true ->
          diag("you appear to have already started the test_server")
    end.

test_server(#test{count=N, pass=Pass, fail=Fail}) ->
    receive
        #test{count=_, pass=P, fail=F} ->
            test_server(#test{count=N+1, pass=Pass+P, fail=Fail+F});
        {Requester, cur_status} ->
            Requester ! #test{count=N, pass=Pass, fail=Fail};
        done ->
            %% output test result information here?
            plan_serv ! stop,
            io:format("Ran ~p Tests Passed: ~p Failed: ~p~n~n", [N-1, Pass, Fail]),
            exit(normal)
    end,
    test_server(#test{count=N, pass=Pass, fail=Fail}).

end_tests() -> test_serv ! done.

%% tap output and registration with test_server
mk_tap(Result, Desc) ->
    test_serv ! {self(), cur_status},
    receive
        #test{count = N, pass = _P, fail = _F} ->
            case Result of
                true ->
                    output(lists:concat(["ok ", N, " -  ",  Desc])),
                    test_serv ! #test{count=null, pass=1, fail=0};
                    
                false ->
                    output(lists:concat(["not ok ", N, " -  ",  Desc])),
                    test_serv ! #test{fail=1}
            end
%        _ ->
%            throw("invalid message from test_serv")
    end.

output(S) -> io:format("~s~n", [S]).
    
diag(S) -> output("# "++S).

ok(Expr, Desc) -> mk_tap(Expr, Desc).

not_ok(Expr, Desc) -> mk_tap(not Expr, Desc).

is(Got, Expected, Desc) -> mk_tap(Got == Expected, Desc).

isnt(Got, Expected, Desc) -> mk_tap(Got /= Expected, Desc).

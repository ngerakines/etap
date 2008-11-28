%% @reference http://testanything.org/wiki/index.php/Main_Page
%% @reference http://en.wikipedia.org/wiki/Test_Anything_Protocol
%% @doc Adds exception based testing to the etap suite.
-module(etap_exception).

-export([dies_ok/2, lives_ok/2, throws_ok/3]).

% ---
% External / Public functions

%% @doc Assert that an exception is raised when running a given function.
dies_ok(F, Desc) ->
    etap:isnt(try_this(F), success, Desc).

%% @doc Assert that an exception is not raised when running a given function.
lives_ok(F, Desc) ->
    etap:is(try_this(F), success, Desc).

%% @doc Assert that the exception thrown by a function matches the given exception.
throws_ok(F, Exception, Desc) ->
    etap:ok((fun(Resp) ->
        case Resp of
            {_, Exception} -> true;
            _ -> false
        end
    end)(try_this(F)), Desc).

% ---
% Internal / Private functions

%% @private
%% @doc Run a function and catch any exceptions.
try_this(F) when is_function(F, 0) ->
    try F() of
        _ -> success
    catch
        throw:E -> {throw, E};
        error:E -> {error, E};
        exit:E -> {exit, E}
    end.

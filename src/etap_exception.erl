%% @reference http://testanything.org/wiki/index.php/Main_Page
%% @reference http://en.wikipedia.org/wiki/Test_Anything_Protocol
%% @doc Adds exception based testing to the etap suite.
-module(etap_exception).

-export([dies_ok/2, lives_ok/2]).

% ---
% External / Public functions

%% @doc Assert that an exception is raised when running a given function.
dies_ok(F, Desc) ->
    etap:is(try_this(F), exception, Desc).

%% @doc Assert that an exception is not raised when running a given function.
lives_ok(F, Desc) ->
    etap:isnt(try_this(F), exception, Desc).

% ---
% Internal / Private functions

%% @private
%% @doc Run a function and catch any exceptions.
try_this(F) when is_function(F, 0) ->
    try F() catch
        throw:_ -> exception;
        error:_ ->exception;
        exit:_ -> exception
    end.

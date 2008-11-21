-module(etap_exception).
-export([dies_ok/2, lives_ok/2]).

-vsn(0.1).
-description("Adds exception based testing to the etap suite").

-import(etap, [ok/2]).

try_this(F) when is_function(F, 0) ->
    try F() catch
        throw:_ -> exception;
        error:_ ->exception;
        exit:_ -> exception
    end.

dies_ok(F, Desc) -> ok(try_this(F) == exception, Desc).

lives_ok(F, Desc) -> ok(not (try_this(F) == exception), Desc).

-module(etap_can).
-export([
    loaded_ok/2, test_loaded/1, try_load/1, can_ok/2, function_exists/2,
    can_ok/3, function_exists/3
]).

-vsn(0.1).
-description("Adds exception based testing to the etap suite").

-import(etap, [ok/2]).

%%% loaded module tests
loaded_ok(M, Desc) when is_atom(M) ->
    ok(test_loaded(M) orelse try_load(M), Desc). % first see if it's loaded then load it

test_loaded(M) -> %% test if it's loaded or not
    case code:is_loaded(M) of
        {file, _Loaded} -> true;
        _ -> false
    end.

try_load(M) -> %try to load it
    case code:load_file(M) of
       {module, _Loaded} -> true;
        _ -> false
    end. 

%% function existance tests
can_ok(M, F) when is_atom(M); is_atom(F) ->
    test_loaded(M) orelse try_load(M),
    ok(function_exists(M, F), lists:concat([M, " can ", F])).

function_exists(M, F) ->
    case lists:keysearch(F, 1, M:module_info(exports)) of
        {value, _} -> true;
        _ -> false
    end. 

%% function/arity existance tests
can_ok(M, F, A) when is_atom(M); is_atom(F), is_number(A) ->
    test_loaded(M) orelse try_load(M),
    ok(function_exists(M, F), lists:concat([M, " can ", F, "/", A])).

function_exists(M, F, A) ->
    L = [X || X <- M:module_info(exports), X == {F,A}],
    case lists:keysearch(F, 1, L) of
        {value, {_, A}} -> true;
        _ -> false
    end. 

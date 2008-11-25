%% @reference http://testanything.org/wiki/index.php/Main_Page
%% @reference http://en.wikipedia.org/wiki/Test_Anything_Protocol
%% @doc Provide test functionality modules
-module(etap_can).

-export([
    loaded_ok/2, can_ok/2, can_ok/3
]).

% ---
% External / Public functions

%% @doc Assert that a module has been loaded successfully.
loaded_ok(M, Desc) when is_atom(M) ->
    etap:ok(test_loaded(M) orelse try_load(M), Desc). % first see if it's loaded then load it

%% @doc Assert that a module exports a given function.
can_ok(M, F) when is_atom(M), is_atom(F) ->
    test_loaded(M) orelse try_load(M),
    etap:ok(function_exists(M, F), lists:concat([M, " can ", F])).

%% @doc Assert that a module exports a given function with a given arity.
can_ok(M, F, A) when is_atom(M); is_atom(F), is_number(A) ->
    test_loaded(M) orelse try_load(M),
    etap:ok(function_exists(M, F, A), lists:concat([M, " can ", F, "/", A])).

% ---
% Internal / Private functions

%% @private
%% @doc Determine if a module has been loaded with code:is_loaded/1
test_loaded(M) ->
    case code:is_loaded(M) of
        {file, _Loaded} -> true;
        _ -> false
    end.

%% @private
%% @doc Attempt to load a module.
try_load(M) ->
    case code:load_file(M) of
       {module, _Loaded} -> true;
        _ -> false
    end.

%% @private
%% @doc Determine if a function exists within a function by
%% looking at it's exported functions.
function_exists(M, F) ->
    case lists:keysearch(F, 1, M:module_info(exports)) of
        {value, _} -> true;
        _ -> false
    end. 

%% @private
%% @doc Determine if a function/arity exists within a function
%% by looking at it's exported functions.
function_exists(M, F, A) ->
    L = [X || X <- M:module_info(exports), X == {F,A}],
    case lists:keysearch(F, 1, L) of
        {value, {_, A}} -> true;
        _ -> false
    end. 

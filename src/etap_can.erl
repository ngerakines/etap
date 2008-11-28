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

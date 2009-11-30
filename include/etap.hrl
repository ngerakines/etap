%% Copyright (c) 2008-2009 Nick Gerakines <nick@gerakines.net>
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

%% @spec ?etap_match(Got, Expected, Desc) -> Result
%%       Got = any()
%%       Expected = any()
%%       Desc = string()
%%       Result = true | false
%% @doc Assert that a value matches a match spec.
-define(etap_match(Got, Expected, Desc),
        etap:expect_fun(fun(XXXXXX) -> case XXXXXX of Expected -> true; _ -> false end end, Got, Desc, ??Expected)).

%% @spec ?etap_throws_match(F, ErrMatch, Desc) -> Result
%%       F = fun()
%%       ErrMatch = any()
%%       Desc = string()
%%       Result = true | false
%% @doc Assert that the exception thrown by a function matches the given exception.
%%      Like etap_exception:throws_ok/3, but with pattern matching
-define(etap_throws_match(F, ErrMatch, Desc),
        try F() of
            _ -> etap:ok(nok, Desc)
        catch
            _:E ->
                ?etap_match(E, ErrMatch, Desc)
        end.

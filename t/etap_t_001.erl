-module(etap_t_001).
-export([start/0]).

-record(tester, {xx, yy, zz}).

start() ->
    etap:plan(13),
    etap:diag("Test the core etap module and it's functions."),
    etap:ok(true, "Proving etap:ok/2 needs true"),
    etap:not_ok(false, "Proving etap:not_ok/2 needs false"),
    etap:is(1, 1, "Proving etap:is/3 must have 2 equal values"),
    etap:isnt(1, 2, "Proving etap:isnt/3 can not have 2 equal values"),
    etap:is([foo, bar, baz], [foo, bar, baz], "Proving etap:is/3 can use complex structures"),
    etap:isnt([foo, bar, baz], [bar, foo, baz], "Proving etap:isnt/3 can use complex structures"),
    
    Foo = #tester{ xx = 1, yy = 2, zz = 3},
    Bar = #tester{ xx = 4, yy = 5, zz = 6},
    
    etap:is(Foo, Foo, "Proving etap:is/3 can use records"),
    etap:isnt(Foo, Bar, "Proving etap:isnt/3 can use records"),
    etap:any(fun(foo) -> true; (_)-> false end, [foo, bar, baz], "Proving etap:any/3 works with atoms"),
    etap:any(fun({_, this}) -> true; (_)-> false end, [{1, none}, {2, all}, {3, this}], "Proving etap:any/3 works with lists of tuples"),
    etap:any(fun([{record, value} | _]) -> true; (_)-> false  end, [[{something, nothing}, {somewhere, nowhere}], [{name, nick}, {has_family, true}], [{record, value}, {term, atom}]], "Proving etap:none/3 works with simple structures"),
    etap:none(fun(the_bitch) -> true; (_)-> false  end, lists:seq(1, 99), "Proving etap:none/3 works with simple structures"),
    etap:none(fun([{color, red} | _]) -> true; (_)-> false  end, [[{something, nothing}, {somewhere, nowhere}], [{name, nick}, {has_family, true}], [{record, value}, {term, atom}]], "Proving etap:none/3 works with simple structures"),
    etap:end_tests().

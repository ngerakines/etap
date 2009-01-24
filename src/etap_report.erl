-module(etap_report).
-export([create/0]).

create() ->
    [cover:import(File) || File <- filelib:wildcard("*coverdata")],
    lists:foreach(
        fun(Module) ->
            file_report(Module)
        end,
        cover:imported_modules()
    ).

file_report(Module) ->
    {ok, Data} = cover:analyse(Module, calls, line),
    Source = find_source(Module),
    case find_source(Module) of
        none -> ok;
        Source ->
            {ok, SourceFD} = file:open(Source, [read]),
            {ok, WriteFD} = file:open(atom_to_list(Module) ++ "_report.html", [write]),
            {Good, Bad} = collect_coverage(Data, {0, 0}),
            io:format(WriteFD, "~s", [header(Module, Good, Bad)]),
            output_lines(Data, WriteFD, SourceFD, 1),
            io:format(WriteFD, "~s", [footer()]),
            file:close(WriteFD),
            file:close(SourceFD),
            ok
    end.

collect_coverage([], Acc) -> Acc;
collect_coverage([{{_, _}, 0} | Data], {Good, Bad}) ->
    collect_coverage(Data, {Good, Bad + 1});
collect_coverage([_ | Data], {Good, Bad}) ->
    collect_coverage(Data, {Good + 1, Bad}).

output_lines(Data, WriteFD, SourceFD, LineNumber) ->
    {Match, NextData} = datas_match(Data, LineNumber),
    case io:get_line(SourceFD, '') of
        eof -> ok;
        Line = "%" ++ _ ->
            io:format(WriteFD, "~s", [out_line(LineNumber, none, Line)]),
            output_lines(NextData, WriteFD, SourceFD, LineNumber + 1);
    	Line ->
    	    case Match of
    	        {true, CC} ->
    	            io:format(WriteFD, "~s", [out_line(LineNumber, CC, Line)]),
    	            output_lines(NextData, WriteFD, SourceFD, LineNumber + 1);
    	        false ->
    	            io:format(WriteFD, "~s", [out_line(LineNumber, none, Line)]),
    	            output_lines(NextData, WriteFD, SourceFD, LineNumber + 1)
    	    end
    end.

out_line(Number, none, Line) ->
    PadNu = string:right(integer_to_list(Number), 5, $.),
    io_lib:format("<span class=\"inferred0\"><a name=\"line~p\"></a>~s ~s</span>", [Number, PadNu, Line]);
out_line(Number, 0, Line) ->
    PadNu = string:right(integer_to_list(Number), 5, $.),
    io_lib:format("<span class=\"uncovered0\"><a name=\"line~p\"></a>~s ~s</span>", [Number, PadNu, Line]);
out_line(Number, _, Line) ->
    PadNu = string:right(integer_to_list(Number), 5, $.),
    io_lib:format("<span class=\"marked0\"><a name=\"line~p\"></a>~s ~s</span>", [Number, PadNu, Line]).

datas_match([], _) -> {false, []};
datas_match([{{_, Line}, CC} | Datas], LineNumber) when Line == LineNumber -> {{true, CC}, Datas};
datas_match(Data, _) -> {false, Data}.


find_source(Module) when is_atom(Module) ->
    Root = filename:rootname(Module),
    Dir = filename:dirname(Root),
    find_source([
        filelib:is_file(Root ++ ".src"),
        filename:join([Dir, "..", "src", Root ++ ".erl"]),
        filename:join([Dir, "src", Root ++ ".erl"])
    ]);
find_source([]) -> none;
find_source([Test | Tests]) ->
    case filelib:is_file(Test) of
        true -> Test;
        false -> find_source(Tests)
    end.

header(Module, Good, Bad) ->
    CovPer = round((Good / (Good + Bad)) * 100),
    io:format("Good ~p~n", [Good]),
    io:format("Bad ~p~n", [Bad]),
    io:format("CovPer ~p~n", [CovPer]),
    io_lib:format("<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\">
        <html lang='en' xml:lang='en' xmlns='http://www.w3.org/1999/xhtml'>
          <head>
            <title>~s - C0 code coverage information</title>
            <style type='text/css'>body { background-color: rgb(240, 240, 245); }</style>
            <style type='text/css'>span.marked0 {
     background-color: rgb(185, 210, 200);
     display: block;
    }
    span.marked1 {
     background-color: rgb(190, 215, 205);
     display: block;
    }
    span.inferred0 {
     background-color: rgb(175, 200, 200);
     display: block;
    }
    span.inferred1 {
     background-color: rgb(180, 205, 205);
     display: block;
    }
    span.uncovered0 {
     background-color: rgb(225, 110, 110);
     display: block;
    }
    span.uncovered1 {
     background-color: rgb(235, 120, 120);
     display: block;
    }
    span.overview {
     border-bottom: 8px solid black;
    }
    div.overview {
     border-bottom: 8px solid black;
    }
    body {
     font-family: verdana, arial, helvetica;
    }
    div.footer {
     font-size: 68%;
     margin-top: 1.5em;
    }
    h1, h2, h3, h4, h5, h6 {
     margin-bottom: 0.5em;
    }
    h5 {
     margin-top: 0.5em;
    }
    .hidden {
     display: none;
    }
    div.separator {
     height: 10px;
    }
    table.percent_graph {
     height: 12px;
     border: #808080 1px solid;
     empty-cells: show;
    }
    table.percent_graph td.covered {
     height: 10px;
     background: #00f000;
    }
    table.percent_graph td.uncovered {
     height: 10px;
     background: #e00000;
    }
    table.percent_graph td.NA {
     height: 10px;
     background: #eaeaea;
    }
    table.report {
     border-collapse: collapse;
     width: 100%;
    }
    table.report td.heading {
     background: #dcecff;
     border: #d0d0d0 1px solid;
     font-weight: bold;
     text-align: center;
    }
    table.report td.heading:hover {
     background: #c0ffc0;
    }
    table.report td.text {
     border: #d0d0d0 1px solid;
    }
    table.report td.value {
     text-align: right;
     border: #d0d0d0 1px solid;
    }
    table.report tr.light {
     background-color: rgb(240, 240, 245);
    }
    table.report tr.dark {
     background-color: rgb(230, 230, 235);
    }
    </style>
          </head>
          <body>
            <h3>C0 code coverage information</h3>
            <p>Generated on ~s with <a href='http://github.com/ngerakines/etap'>etap 0.3.3</a>.
            </p>            
        <table class='report'>
          <thead>
            <tr>
              <td class='heading'>Name</td>
              <td class='heading'>Total lines</td>
              <td class='heading'>Lines of code</td>
              <td class='heading'>Total coverage</td>
              <td class='heading'>Code coverage</td>
            </tr>
          </thead>
          <tbody>
            <tr class='light'>

              <td>
                <a href='~s'>~s</a>
              </td>
              <td class='value'>
                <tt>??</tt>
              </td>
              <td class='value'>
                <tt>??</tt>
              </td>
              <td class='value'>
                <tt>??</tt>
              </td>
              <td>
                <table cellspacing='0' cellpadding='0' align='right'>
                  <tr>
                    <td><tt>~p%</tt>&nbsp;</td><td>
                      <table cellspacing='0' class='percent_graph' cellpadding='0' width='100'>
                      <tr><td class='covered' width='~p' /><td class='uncovered' width='~p' /></tr>
                      </table>
                    </td>
                  </tr>
                </table>
              </td>
            </tr>
          </tbody>
        </table><pre>", [Module, etap:datetime({date(), time()}), atom_to_list(Module) ++ "_report.html", Module, round(CovPer), Good, Bad]).

footer() ->
    "</pre><hr /><p>Generated using <a href='http://github.com/ngerakines/etap'>etap 0.3.3</a>.</p>
          </body>
        </html>
    ".

-module(etap_t_005).
-export([start/0]).

start() ->
    etap:plan(4),
    etap:start_ok(inets, "Application inets starts"),
    etap:simple_200("http://socklabs.com/", "Web request against http://socklabs.com/ returns a 200"),
    etap:simple_404("http://socklabs.com/404.html", "Web request against http://socklabs.com/404.html returns a 404"),
    
    Request = etap:build_request(get, "http://socklabs.com/", [], []),
    Request:status_is(200, "Web request against http://socklabs.com/ returns a 200"),
    
    etap:end_tests().

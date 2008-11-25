-module(etap_t_005).
-export([start/0]).

start() ->
    etap:plan(6),
    etap_application:start_ok(inets, "Application inets starts"),
    etap_web:simple_200("http://socklabs.com/", "Web request against http://socklabs.com/ returns a 200"),
    etap_web:simple_404("http://socklabs.com/404.html", "Web request against http://socklabs.com/404.html returns a 404"),
    
    Request = etap_web:build_request(get, "http://socklabs.com/", [], []),
    Request:status_is(200, "Web request against http://socklabs.com/ returns a 200"),
    
    etap:end_tests().

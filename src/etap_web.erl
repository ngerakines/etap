%% @author Nick Gerakines <nick@gerakines.net> [http://socklabs.com/]
%% @copyright 2008 Nick Gerakines
%% @todo Support cookies.
%% @doc Provide testing functionality for web requests.
-module(etap_web).

-export([simple_200/2, simple_404/2, build_request/4]).

%% @doc Fetch a url and verify that it returned a 200 status.
simple_200(Url, Desc) ->
    Request = build_request(get, Url, [], []),
    etap:ok(Request:status_code() == 200, Desc).

%% @doc Fetch a url and verify that it returned a 404 status.
simple_404(Url, Desc) ->
    Request = build_request(get, Url, [], []),
    etap:ok(Request:status_code() == 404, Desc).

%% @doc Create and return a request structure.
build_request(Method, Url, Headers, Body) ->
    try http:request(Method, {Url, Headers}, [], []) of
        {ok, {OutStatus, OutHeaders, OutBody}} ->
            etap_request:new(Method, Url, Headers, Body, OutStatus, OutHeaders, OutBody);
        _ -> error
    catch
        _:_ -> error
    end.

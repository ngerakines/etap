-module(etap_web).

-export([load_ok/4, request/4]).

-vsn(0.1).
-description("Adds interweb based testing to the etap suite").

-import(etap, [ok/2]).

load_ok(Url, Headers, Body, Desc) ->
    Request = request(get, Url, Headers, Body),
    ok(Request:get(status) == 200, Desc).

%% Req = etap_request:create(get, "http://google.com/", [], []).
request(Method, Url, Headers, Body) ->
    try http:request(get, {Url, Headers}, [], []) of
        {ok, {OutStatus, OutHeaders, OutBody}} ->
            etap_request:new(Method, Url, Headers, Body, OutStatus, OutHeaders, OutBody);
        Result -> error
    catch
        _:_ -> error
    end.

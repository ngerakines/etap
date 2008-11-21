-module(etap_request, [Method, Url, InHeaders, InBody, Status, OutHeaders, OutBody]).
-author('Nick Gerakines <nick@gerakines.net>').

-export([
    method/0,
    url/0,
    status/0, status_code/0, status_line/0,
    rheaders/0, has_rheader/1, rheader/1,
    rbody/0
]).

method() -> Method.

url() -> Url.

status() -> Status.

status_code() ->
    {_, Code, _} = Status,
    Code.

status_line() ->
    {_, _, Line} = Status,
    Line.

rheaders() -> OutHeaders.

has_rheader(Key) ->
    lists:keymember(Key, 1, OutHeaders).

rheader(Key) ->
    case lists:keysearch(Key, 1, OutHeaders) of
        false -> undefined;
        {value, {Key, Value}} -> Value
    end.

rbody() -> OutBody.

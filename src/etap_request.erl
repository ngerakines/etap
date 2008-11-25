%% @doc Provides test functionality against a specific web request. Many of
%% the exported methods can be used to build your own more complex tests.
-module(etap_request, [Method, Url, InHeaders, InBody, Status, OutHeaders, OutBody]).

-export([status_is/2]).

-export([
    method/0, url/0, status/0, status_code/0, status_line/0, rheaders/0,
    has_rheader/1, rheader/1, rbody/0
]).

% ---
% Tests

status_is(Code, Desc) ->
    etap:is(Code, status_code(), Desc).

% ---
% Accessor functions

%% @doc Access a request's method.
method() -> Method.

%% @doc Access a request's URL.
url() -> Url.

%% @doc Access a request's status.
status() -> Status.

%% @doc Access a request's status code.
status_code() ->
    {_, Code, _} = Status,
    Code.

%% @doc Access a request's status line.
status_line() ->
    {_, _, Line} = Status,
    Line.

%% @doc Access a request's headers.
rheaders() -> OutHeaders.

%% @doc Dertermine if a specific request header exists.
has_rheader(Key) ->
    lists:keymember(Key, 1, OutHeaders).

%% @doc Return a specific request header.
rheader(Key) ->
    case lists:keysearch(Key, 1, OutHeaders) of
        false -> undefined;
        {value, {Key, Value}} -> Value
    end.

%% @doc Access the request's body.
rbody() -> OutBody.

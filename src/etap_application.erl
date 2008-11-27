%% @author Nick Gerakines <nick@gerakines.net> [http://socklabs.com/]
%% @copyright 2008 Nick Gerakines
%% @reference http://testanything.org/wiki/index.php/Main_Page
%% @reference http://en.wikipedia.org/wiki/Test_Anything_Protocol
%% @todo Explain in documentation why we use a process to handle test input.
%% @todo Add test to verify the number of members in a pg2 group.
%% @doc Provide test functionality to the application and related behaviors.
-module(etap_application).
-export([
    start_ok/2, ensure_loaded/3, load_ok/2,
    pg2_group_exists/2, pg2_group_doesntexist/2
]).

%% @doc Assert that an application can be loaded successfully.
load_ok(AppName, Desc) ->
    etap:ok(application:load(AppName) == ok, Desc).

%% @doc Assert that an application can be started successfully.
start_ok(AppName, Desc) ->
    etap:ok(application:start(AppName) == ok, Desc).

%% @doc Assert that an application has been loaded successfully.
ensure_loaded(AppName, AppVsn, Desc) ->
    etap:any(
        fun(Match) -> case Match of {AppName, _, AppVsn} -> true; _ -> false end end,
        application:loaded_applications(),
        Desc
    ).

%% @doc Assert that a pg2 group exists.
pg2_group_exists(GroupName, Desc) ->
    etap:any(
        fun(Match) -> Match == GroupName end,
        pg2:which_groups(),
        Desc
    ).

%% @doc Assert that a pg2 group does not exists.
pg2_group_doesntexist(GroupName, Desc) ->
    etap:none(
        fun(Match) -> Match == GroupName end,
        pg2:which_groups(),
        Desc
    ).

-module(etap_application).

-export([start_ok/2, ensure_loaded/3, pg2_group_exists/2, pg2_group_doesntexis/2, load_ok/2]).

-author('Nick Gerakines <nick@gerakines.net>').
-vsn(0.1).
-description("Adds application based testing to the etap suite").

-import(etap, [ok/2]).

start_ok(AppName, Desc) ->
    ok(application:start(AppName) == ok, Desc).

load_ok(AppName, Desc) ->
    ok(application:load(AppName) == ok, Desc).

ensure_loaded(AppName, AppVsn, Desc) ->
    LoadedApps = application:loaded_applications(),
    ok(
        lists:any(
            fun (Elem) ->
                case Elem of
                    {AppName, _, AppVsn} -> true;
                    _ -> false
                end
            end,
            LoadedApps
        ) == true,
        Desc
    ).

pg2_group_exists(GroupName, Desc) ->
    ok(
        lists:any(
            fun (Elem) -> GroupName == Elem end,
            pg2:which_groups()
        ) == true,
        Desc
    ).

pg2_group_doesntexist(GroupName, Desc) ->
    ok(
        lists:any(
            fun (Elem) -> GroupName == Elem end,
            pg2:which_groups()
        ) == false,
        Desc
    ).

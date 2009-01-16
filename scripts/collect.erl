#!/usr/bin/env escript
%% -*- erlang -*-
%%! -noshell

main(_) ->
    [cover:import(File) || File <- filelib:wildcard("*.coverdata")],
    lists:foreach(
        fun(Module) ->
            cover:analyse_to_file(
                Module, atom_to_list(Mod) ++ "_coverage.txt", []
            )
        end,
        cover:imported_modules()
    ).


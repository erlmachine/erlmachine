-module(erlmachine_datasheet).

%% This module is responsible to:
%% a) To read technical specifications (datasheets);
%% b) To validate datasheet content via https://json-schema.org;
%% c) To build extensions accordingly to datasheets;

%% API.
-export([datasheet/1]).
-export([schema/0]).

-export([gear/1, gear/2]).
-export([shaft/1, shaft/2]).
-export([axle/1, axle/2]).
-export([gearbox/2, gearbox/3]).

-export([validate/1]).

-type datasheet() :: map().

-export_type([datasheet/0]).

-include("erlmachine_factory.hrl").
-include("erlmachine_assembly.hrl").
-include("erlmachine_system.hrl").

-spec schema() -> list().
schema() ->
    "datasheet".

-spec validate(Datasheet::map()) -> success(map()) | failure(term()).
validate(Datasheet) ->
    jesse:validate(schema(), Datasheet).

-spec datasheet(Path::list()) ->
                  success(datasheet()) | failure(term(), term()).
datasheet(Path) ->
    try
        Opt = [{str_node_as_binary, true}, {map_node_format, map}],
        [Res] = yamerl:decode_file(Path, Opt), {ok, _} = validate(Res)
    catch E:R ->
            erlmachine:failure(E, R)
    end.

%% Datasheet has to be preloaded and passed as argument;
-spec gear(Datasheet::datasheet()) -> 
                  success(assembly()) | failure(term(), term()).
gear(Datasheet) ->
    gear(Datasheet, []).

-spec gear(Datasheet::datasheet(), Exts::list(assembly())) -> 
                  success(assembly()) | failure(term(), term()).
gear(Datasheet, Exts) ->
    io:format("~nDatasheet: ~p~n",[Datasheet]),
    Name = test, Opt = [], ProtName = test, ProtOpt = [],
    erlmachine_factory:gear(Name, Opt, ProtName, ProtOpt, Exts).

-spec shaft(Datasheet::datasheet()) ->
                   success(assembly()) | failure(term(), term()).
shaft(Datasheet) ->
    shaft(Datasheet, []).

-spec shaft(Datasheet::datasheet(), Exts::list(assembly())) ->
                   success(assembly()) | failure(term(), term()).
shaft(Datasheet, Exts) ->
    io:format("~nDatasheet: ~p~n",[Datasheet]),
    Name = test, Opt = [], ProtName = test, ProtOpt = [],
    erlmachine_factory:shaft(Name, Opt, ProtName, ProtOpt, Exts).

-spec axle(Datasheet::datasheet()) ->
                  success(assembly()) | failure(term(), term()).
axle(Datasheet) ->
    axle(Datasheet, []).

-spec axle(Datasheet::datasheet(), Exts::list(assembly())) -> 
                  success(assembly()) | failure(term(), term()).
axle(Datasheet, Exts) ->
    io:format("~nDatasheet: ~p~n",[Datasheet]),
    Name = test, Opt = [], ProtName = test, ProtOpt = [],
    erlmachine_factory:axle(Name, Opt, ProtName, ProtOpt, Exts).

-spec gearbox(Datasheet::datasheet(), Env::term()) ->
                     success(assembly()) | failure(term(), term()).
gearbox(Datasheet, Env) ->
    gearbox(Datasheet, Env, []).

-spec gearbox(Datasheet::datasheet(), Env::term(), Exts::list(assembly())) -> 
                     success(assembly()) | failure(term(), term()).
gearbox(Datasheet, Env, Exts) ->
    io:format("~nDatasheet: ~p~n",[Datasheet]),
    Name = test, Opt = [], ProtName = test, ProtOpt = [],
    erlmachine_factory:gearbox(Name, Opt, ProtName, ProtOpt, Env, Exts).

%% TODO to support Json based specification;

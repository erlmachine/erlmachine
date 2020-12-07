-module(erlmachine_datasheet).

%% This module is responsible to:
%% a) To read technical specifications (datasheets);
%% b) To validate datasheet content via https://json-schema.org;
%% c) To build extensions accordingly to datasheets;

%% NOTE: There is possibility to load datasheet from other sources (for example DB);
%% But validate/1 call should be performed before usage; 

%% API.
-export([new/0]).

-export([file/1]).
-export([decode/2]).
-export([schema/0]).

-export([validate/1]).

-export([iterator/1, next/1]).
-export([get/2]).

-type datasheet() :: map().

-export_type([datasheet/0]).

-include("erlmachine_assembly.hrl").
-include("erlmachine_system.hrl").

%% NOTE: This module should provide access  to the datasheet fields for factory;
%% By the same way as assembly;

-spec schema() -> list().
schema() ->
    "datasheet".

-spec new() -> datasheet().
new() ->
    maps:new().

-spec get(Field::term(), Datasheet::datasheet()) -> term().
get(Field, Datasheet) ->
    maps:get(Field, Datasheet).

-spec validate(Datasheet::map()) -> success(map()) | failure(term()).
validate(Datasheet) ->
    jesse:validate(schema(), Datasheet).


-spec decode(Spec::list(), Opt::list()) ->
                    success(datasheet()) | failure(term(), term()).
decode(Spec, Opt) ->
    try
        [Res] = yamerl:decode(Spec, Opt), {ok, _} = validate(Res)
    catch E:R ->
            erlmachine:failure(E, R)
    end.

-spec file(Path::list()) ->
                  success(datasheet()) | failure(term(), term()).
file(Path) ->
    try
        Opt = [{str_node_as_binary, true}, {map_node_format, map}],
        [Res] = yamerl_constr:file(Path, Opt), {ok, _} = validate(Res)
    catch E:R ->
            erlmachine:failure(E, R)
    end.

-spec iterator(Datasheet::datasheet()) -> term().
iterator(Datasheet) ->
    maps:iterator(Datasheet).

-spec next(Iterator::term()) -> none | {Key::binary(), Value::term(), Iterator::term()}.
next(Iterator) ->
    maps:next(Iterator).

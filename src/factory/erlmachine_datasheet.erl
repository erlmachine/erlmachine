-module(erlmachine_datasheet).

%% This module is responsible to:
%% a) Read technical manufacturer's specifications (datasheets);
%% b) Validate datasheet content via https://json-schema.org;
%% c) Create assembly accordingly to the extension datasheet;
%% d) Create graph accordingly to the transmission datasheet

%% NOTE: There is possibility to load datasheet from other sources (via DB, network, etc.);

%% API.
-export([assembly/1, transmission/1]).

-export([file/2]).
-export([decode/3]).

-export([new/0]).

-export([iterator/1, next/1]).
-export([find/2]).

-opaque datasheet() :: map().

-type path() :: list().
-type spec() :: list().

-export_type([datasheet/0]).

-include("erlmachine_assembly.hrl").
-include("erlmachine_system.hrl").

-spec assembly(Path::path()) ->
                      success(datasheet()) | failure(term(), term()).
assembly(Path) ->
    file(Path, "assembly.json").

-spec transmission(Path::path()) ->
                          success(datasheet()) | failure(term(), term()).
transmission(Path) ->
    file(Path, "transmission.json").

-spec file(Path::path(), Schema::[term()]) ->
                  success(datasheet()) | failure(term(), term()).
file(Path, Schema) ->
    try
        Opt = [{str_node_as_binary, true}, {map_node_format, map}],
        [Datasheet] = yamerl_constr:file(Path, Opt),
        {ok, _} = jesse:validate(Schema, Datasheet)
    catch E:R ->
            erlmachine:failure(E, R)
    end.

-spec decode(Spec::spec(), Opt::[term()], Schema::[term()]) ->
                    success(datasheet()) | failure(term(), term()).
decode(Spec, Opt, Schema) ->
    try
        [Datasheet] = yamerl:decode(Spec, Opt),
        {ok, _} = jesse:validate(Schema, Datasheet)
    catch E:R ->
            erlmachine:failure(E, R)
    end.

-spec new() -> datasheet().
new() ->
    maps:new().

-spec iterator(Datasheet::datasheet()) -> term().
iterator(Datasheet) ->
    maps:iterator(Datasheet).

-spec next(Iterator::term()) -> none | {Key::binary(), Value::term(), Iterator::term()}.
next(Iterator) ->
    maps:next(Iterator).

-spec find(Field::term(), Datasheet::datasheet()) -> success(term()) | failure().
find(Field, Datasheet) ->
    maps:find(Field, Datasheet).

-module(erlmachine_datasheet).

%% This module is responsible to:
%% a) To read technical specifications (datasheets);
%% b) To validate datasheet content via https://json-schema.org;
%% c) To build extensions accordingly to datasheets;

%% NOTE: There is possibility to load datasheet from other sources (for example DB);
%% But validate/1 call should be performed before usage; 

%% API.
-export([file/1]).
-export([decode/2]).
-export([schema/0]).

-export([validate/1]).

-type datasheet() :: map().

-export_type([datasheet/0]).

-include("erlmachine_system.hrl").

-spec schema() -> list().
schema() ->
    "datasheet".

-spec validate(Datasheet::map()) -> success(map()) | failure(term()).
validate(Datasheet) ->
    jesse:validate(schema(), Datasheet).


-spec decode(Spec::list(), Opts::list()) ->
                    success(datasheet()) | failure(term(), term()).
decode(Spec, Opts) ->
    try
        [Res] = yamerl:decode(Spec, Opts), {ok, _} = validate(Res)
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


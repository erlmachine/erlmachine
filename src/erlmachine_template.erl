-module(erlmachine_template).

%% This module is responsible:

%% - Read YAML specifications (templates);
%% - Validate template content against https://json-schema.org;

%% NOTE: There is a potential possibility to load template from other sources such DB, network, etc.
-export([is_template/1]).

-export([add_schema/1]).

-export([file/2]).
-export([decode/3]).

-export([new/0]).

-export([iterator/1, next/1]).
-export([find/2]).

-opaque template() :: map().

-type path() :: list().
-type spec() :: list().

-export_type([template/0]).

-callback schema() -> term().
-callback file() -> path().

-include("erlmachine_system.hrl").

%%% Modules

-spec is_template(Module::atom()) -> boolean().
is_template(Module) ->
    lists:member(?MODULE, erlmachine:behaviours(Module)).

-spec add_schema(Module::atom()) -> success().
add_schema(Module) ->
    Schema = Module:schema(), File = Module:file(),

    [Json] = jsx:consult(File, [return_maps]), ok = jesse:add_schema(Schema, Json).

-spec file(Module::atom(), Path::path()) ->
                  success(template()) | failure(term(), term()).
file(Module, Path) ->
    Schema = Module:schema(),

    try
        Opt = [{ 'str_node_as_binary', true }, { 'map_node_format', map }],
        [Template] = yamerl_constr:file(Path, Opt),

        {ok, _} = jesse:validate(Schema, Template)
    catch E:R ->
            erlmachine:failure(E, R)
    end.

-spec decode(Module::module(), Spec::spec(), Opt::[term()]) ->
                    success(template()) | failure(term(), term()).
decode(Module, Spec, Opt) ->
    Schema = Module:schema(),

    try
        [Template] = yamerl:decode(Spec, Opt),

        {ok, _} = jesse:validate(Schema, Template)
    catch E:R ->
            erlmachine:failure(E, R)
    end.

-spec new() -> template().
new() ->
    maps:new().

-spec iterator(Template::template()) -> term().
iterator(Template) ->
    maps:iterator(Template).

-spec next(Iterator::term()) -> none | {binary(), term(), term()}.
next(Iterator) ->
    maps:next(Iterator).

-spec find(Field::term(), Template::template()) -> success(term()) | failure().
find(Field, Template) ->
    maps:find(Field, Template).

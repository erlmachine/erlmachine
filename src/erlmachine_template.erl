-module(erlmachine_template).
%% NOTE: There is a potential possibility to load template from other sources: DB, network, etc.
%% NOTE: This module is responsible:

%% - Read YAML specifications (templates);
%% - Validate template content against https://json-schema.org;

-export([is_template/1, templates/1]).

-export([add_schema/1]).

-export([validate/2]).

-export([file/2, file/3]).
-export([decode/3]).

-export([new/0]).

-export([iterator/1, next/1]).

-export([find/2]).
-export([get/2]).

-type field() :: binary().
-type value() :: binary() | [term()] | integer() | map() | float() | null.

-type template() :: map().
-type iterator() :: term().

-type path() :: list().
-type spec() :: list().

-export_type([template/0, iterator/0]).

-callback schema() -> term().
-callback file() -> path().

-include_lib("erlbox/include/erlbox.hrl").

%%% Modules API

-spec is_template(Module::atom()) -> boolean().
is_template(Module) ->
    lists:member(?MODULE, erlmachine:behaviours(Module)).

-spec templates(Modules::[module()]) -> [module()].
templates(Modules) ->
    [M || M <- Modules, is_template(M)].

%%% Template API

-spec add_schema(Module::atom()) -> success().
add_schema(Module) ->
    Schema = Module:schema(), File = Module:file(),

    [Json] = jsx:consult(File, [return_maps]), ok = jesse:add_schema(Schema, Json).


-spec validate(Module::atom(), T::template()) ->
                      success(template()) | failure(term()).
validate(Module, T) ->
    Schema = Module:schema(),

    jesse:validate(Schema, T).

-spec file(Module::atom(), Path::path()) ->
                  success(template()) | failure(term(), term()).
file(Module, Path) ->
    Opt = [{ 'str_node_as_binary', true }, { 'map_node_format', map }],

    file(Module, Path, Opt).

-spec file(Module::atom(), Path::path(), Opt::[term()]) ->
                  success(template()) | failure(term(), term()).
file(Module, Path, Opt) ->
    try
        [T] = yamerl_constr:file(Path, Opt), {ok, _} = validate(Module, T)
    catch E:R ->
            erlmachine:failure(E, R)
    end.

-spec decode(Module::module(), Spec::spec(), Opt::[term()]) ->
                    success(template()) | failure(term(), term()).
decode(Module, Spec, Opt) ->
    try
        [T] = yamerl:decode(Spec, Opt), {ok, _} = validate(Module, T)
    catch E:R ->
            erlmachine:failure(E, R)
    end.

-spec new() -> template().
new() ->
    maps:new().

-spec iterator(T::template()) -> iterator().
iterator(T) ->
    Res = maps:iterator(T),
    Res.

-spec next(I::iterator()) -> none | {field(), value(), iterator()}.
next(I) ->
    Res = maps:next(I),
    Res.

-spec find(Field::field(), T::template()) -> success(value()) | failure().
find(Field, T) ->
    Res = maps:find(Field, T),
    Res.

-spec get(Field::field(), T::template()) -> value().
get(Field, T) ->
    Res = maps:get(Field, T),
    Res.

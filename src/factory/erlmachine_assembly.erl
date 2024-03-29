-module(erlmachine_assembly).

-behaviour(erlmachine_factory).
-behaviour(erlmachine_template).

%% API.

-export([schema/0]).
-export([file/0]).

-export([is_assembly/1]).

-export([new/0, new/5]).

-export([
         id/1, id/2,
         serial_no/1, serial_no/2,
         type/1, type/2,
         body/0, body/1, body/2,
         model_no/1, model_no/2,
         port/1, port/2,
         graph/1, graph/2,
         model/1, model/2,
         prototype/1, prototype/2,
         extensions/0, extensions/1, extensions/2,
         uid/1, uid/2,
         tags/1, tags/2,
         vertex/1, vertex/2,
         part_no/1, part_no/2,
         env/0, env/1, env/2,
         description/1, description/2
        ]).

-export([template/1]).
-export([process/2]).

-export([to_json/1, from_json/1]).
-export([to_binary/1, from_binary/1]).

-include_lib("erlbox/include/erlbox.hrl").

-include("erlmachine_template.hrl").
-include("erlmachine_user.hrl").
-include("erlmachine_factory.hrl").
-include("erlmachine_graph.hrl").

%% NOTE: Here is implemented incapsulation across independent parts and the whole transmission too;
%% We consider model and prototype as implementation parts (like classes) and serial number as unique id of an instance (object);
%% We can support polymorphism by different ways - by overriding prototype or by changing model itself;

%% NOTE: I am thinking about two kinds of assembly manual and automated;
%% Manual can be produced on canvas whereas automated via function call

-record (assembly, {
                    %% Produced number which is assigned by factory
                    id::integer(),
                    %% Runtime unique identifier (S/N)
                    serial_no::serial_no(),
                    %% Behavioral type of an extension
                    type::type(),
                    %% Data structure that stores the state of an extension
                    body = #{}::map() | [term()],
                    %% Product configurator input to generate a master production schedule
                    model_no::model_no(),
                    %% Conenction port of an extension (accordingly to J.P. Morrison)
                    port::term(),
                    %% Build topology which is inherited through the all extensions
                    graph::graph(),
                    %% Domain level implementation
                    model::model(),
                    %% Service level implementation
                    prototype::prototype(),
                    %% Build configuration
                    extensions = []::[] | [assembly()],
                    %% The identity of Erlmachine operator
                    uid::uid(),
                    %% Index terms which are assigned as meta-information
                    tags = []::[] | [term()],
                    %% The identity on a graph (by default serial_no)
                    vertex::vertex(),
                    %% Deployment identity to track the quality of a component through release period
                    part_no::part_no(),
                    %% The environment context which is passed to the domain level implementation
                    env = #{}::map(),
                    %% Short textual overview or metadata for auto generated extension
                    description::binary()
                   }
        ).

-type type() :: 'worker' | 'supervisor'.

-type model() :: erlmachine_model:model().
-type prototype() :: erlmachine_prototype:prototype().

-type assembly() :: #assembly{}.

-type json() :: map().

-export_type([assembly/0]).

-spec is_assembly(Term::term()) -> boolean().
is_assembly(Term) ->
    is_record(Term, assembly).

-spec new() -> assembly().
new() ->
    #assembly{}.

-spec new(ModelName::atom(), ModelOpt::map(), ProtName::atom(), ProtOpt::map(), Env::map()) -> 
                 assembly().
new(ModelName, ModelOpt, ProtName, ProtOpt, Env) ->
    Assembly = new(),

    Model = erlmachine_model:new(ModelName, ModelOpt),
    Prototype = erlmachine_prototype:new(ProtName, ProtOpt),

    Rel = prototype(model(Assembly, Model), Prototype),
    env(Rel, Env).

-spec template(Path::path()) ->
                      success(template()) | failure(term(), term()).
template(Path) ->
    erlmachine_template:file(?MODULE, Path).

%%% erlmachine_template

-spec schema() -> list().
schema() ->
    "assembly.json".

-spec file() -> path().
file() ->
    Priv = erlmachine:priv_dir(), File = schema(),
    filename:join(Priv, File).

%%% erlmachine_factory

-spec process(Assembly::assembly(), T::template()) -> assembly().
process(Assembly, T) ->
    I = erlmachine_template:iterator(T), Next = erlmachine_template:next(I),

    Res = next(Assembly, Next),
    Res.

-spec next(Assembly::assembly(), none | {binary(), term(), term()}) ->
                  assembly().
next(Assembly, none) ->
    Assembly;

next(Assembly, {<<"serial_no">>, V, I}) ->
    Rel = serial_no(Assembly, V),

    Res = next(Rel, erlmachine_template:next(I)),
    Res;

next(Assembly, {<<"type">>, V, I}) ->
    Rel = type(Assembly, binary_to_atom(V)),

    Res = next(Rel, erlmachine_template:next(I)),
    Res;

next(Assembly, {<<"body">>, V, I}) ->
    Rel = body(Assembly, V),

    Res = next(Rel, erlmachine_template:next(I)),
    Res;

next(Assembly, {<<"model_no">>, V, I}) ->
    Rel = model_no(Assembly, V),

    Res = next(Rel, erlmachine_template:next(I)),
    Res;

next(Assembly, {<<"port">>, V, I}) ->
    Rel = port(Assembly, V),

    Res = next(Rel, erlmachine_template:next(I)),
    Res;

next(Assembly, {<<"model">>, V, I}) ->
    Name = erlmachine_template:get(<<"module">>, V),
    Module = binary_to_atom(Name),

    Model =
        case erlmachine_template:find(<<"options">>, V) of
            {ok, Opt} ->
                erlmachine_model:new(Module, Opt);
            _ ->
                erlmachine_model:new(Module)
        end,

    Rel = model(Assembly, Model),

    Res = next(Rel, erlmachine_template:next(I)),
    Res;

next(Assembly, {<<"prototype">>, V, I}) ->
    Name = erlmachine_template:get(<<"module">>, V),
    Module = binary_to_atom(Name),

    Prot =
        case erlmachine_template:find(<<"options">>, V) of
            {ok, Opt} ->
                erlmachine_prototype:new(Module, Opt);
            _ ->
                erlmachine_prototype:new(Module)
        end,

    Rel = prototype(Assembly, Prot),

    Res = next(Rel, erlmachine_template:next(I)),
    Res;

next(Assembly, {<<"uid">>, V, I}) ->
    Rel = uid(Assembly, V),

    Res = next(Rel, erlmachine_template:next(I)),
    Res;

next(Assembly, {<<"tags">>, V, I}) ->
    Rel = tags(Assembly, V),

    Res = next(Rel, erlmachine_template:next(I)),
    Res;

next(Assembly, {<<"vertex">>, V, I}) ->
    Rel = vertex(Assembly, V),

    Res = next(Rel, erlmachine_template:next(I)),
    Res;

next(Assembly, {<<"part_no">>, V, I}) ->
    Rel = part_no(Assembly, V),

    Res = next(Rel, erlmachine_template:next(I)),
    Res;

next(Assembly, {<<"env">>, V, I}) ->
    Rel = env(Assembly, V),

    Res = next(Rel, erlmachine_template:next(I)),
    Res;

next(Assembly, {<<"description">>, V, I}) ->
    Rel = description(Assembly, V),

    Res = next(Rel, erlmachine_template:next(I)),
    Res;

next(Assembly, {_, _, I}) ->
    Res = next(Assembly, erlmachine_template:next(I)),
    Res.

%%% Field accessors

-spec id(Assembly::assembly()) -> integer().
id(Assembly) ->
    Assembly#assembly.id.

-spec id(Assembly::assembly(), Id::integer()) -> assembly().
id(Assembly, Id) ->
    Assembly#assembly{ 'id' = Id }.

-spec serial_no(Assembly::assembly()) -> serial_no().
serial_no(Assembly) ->
    Assembly#assembly.serial_no.

-spec serial_no(Assembly::assembly(), SN::serial_no()) -> assembly().
serial_no(Assembly, SN) ->
    Assembly#assembly{ 'serial_no' = SN }.

-spec type(Assembly::assembly()) -> type().
type(Assembly) ->
    Assembly#assembly.type.

-spec type(Assembly::assembly(), Type::type()) -> assembly().
type(Assembly, Type) ->
    Assembly#assembly{ 'type' = Type }.

-spec body() -> map().
body() ->
    #assembly{}#assembly.body.

-spec body(Assembly::assembly()) -> map() | [term()].
body(Assembly) ->
    Assembly#assembly.body.

-spec body(Assembly::assembly(), Body::map() | [term()]) -> assembly().
body(Assembly, Body) ->
    Assembly#assembly{ 'body' = Body }.

-spec model_no(Assembly::assembly()) -> model_no().
model_no(Assembly) ->
    Assembly#assembly.model_no.

-spec model_no(Assembly::assembly(), MN::model_no()) -> assembly().
model_no(Assembly, MN) ->
    Assembly#assembly{ 'model_no' = MN }.

-spec port(Assembly::assembly()) -> term().
port(Assembly) -> 
    Assembly#assembly.port.

-spec port(Assembly::assembly(), Port::term()) -> assembly().
port(Assembly, Port) ->
    Assembly#assembly{ 'port' = Port }.

-spec graph(Assembly::assembly()) -> graph().
graph(Assembly) ->
    Assembly#assembly.graph.

-spec graph(Assembly::assembly(), Graph::graph()) -> assembly().
graph(Assembly, Graph) ->
    Assembly#assembly{ 'graph' = Graph }.

-spec model(Assembly::assembly()) -> model().
model(Assembly) ->
    Assembly#assembly.model.

-spec model(Assembly::assembly(), Model::model()) -> assembly().
model(Assembly, Model) ->
    Assembly#assembly{ 'model' = Model }.

-spec prototype(Assembly::assembly()) -> prototype().
prototype(Assembly) ->
    Assembly#assembly.prototype.

-spec prototype(Assembly::assembly(), Prot::prototype()) -> assembly().
prototype(Assembly, Prot) ->
    Assembly#assembly{ 'prototype' = Prot }.

-spec extensions() -> [].
extensions() ->
    #assembly{}#assembly.extensions.

-spec extensions(Assembly::assembly()) -> [] | [assembly()].
extensions(Assembly) ->
    Assembly#assembly.extensions.

-spec extensions(Assembly::assembly(), Exts::[] | [assembly()]) -> assembly().
extensions(Assembly, Exts) when is_list(Exts) ->
    [true = is_assembly(Ext)|| Ext <- Exts],

    Assembly#assembly{ 'extensions' = Exts }.

-spec uid(Assembly::assembly()) -> uid().
uid(Assembly) ->
    Assembly#assembly.uid.

-spec uid(Assembly::assembly(), UID::uid()) -> assembly().
uid(Assembly, UID) ->
    true = is_integer(UID),

    Assembly#assembly{ 'uid' = UID }.

-spec part_no(Assembly::assembly()) -> term().
part_no(Assembly) ->
    Assembly#assembly.part_no.

-spec part_no(Assembly::assembly(), PN::term()) -> assembly().
part_no(Assembly, PN) ->
    Assembly#assembly{ 'part_no' = PN }.

-spec tags(Assembly::assembly()) -> term().
tags(Assembly) ->
    Assembly#assembly.tags.

-spec tags(Assembly::assembly(), Tags::term()) -> assembly().
tags(Assembly, Tags) when is_list(Tags) ->
    Assembly#assembly{ 'tags' = Tags }.

-spec vertex(Assembly::assembly()) -> term().
vertex(Assembly) ->
    Assembly#assembly.vertex.

-spec vertex(Assembly::assembly(), Vertex::term()) -> assembly().
vertex(Assembly, Vertex) ->
    Assembly#assembly{ 'vertex' = Vertex }.

-spec env() -> map().
env() ->
    #assembly{}#assembly.env.

-spec env(Assembly::assembly()) -> map().
env(Assembly) ->
    Assembly#assembly.env.

-spec env(Assembly::assembly(), Env::map()) -> assembly().
env(Assembly, Env) ->
    true = is_map(Env),

    Assembly#assembly{ 'env' = Env }.

-spec description(Assembly::assembly()) -> binary().
description(Assembly) ->
    Assembly#assembly.description.

-spec description(Assembly::assembly(), Desc::binary()) -> assembly().
description(Assembly, Desc) ->
    Assembly#assembly{ 'description' = Desc }.

%%% Format API

%% TODO Unified format behaviour (erlmachine_format, etc.)
%% TODO:

-spec to_json(Assembly::assembly()) -> json().
to_json(_Assembly) ->
    throw(?LINE).

-spec from_json(json()) -> assembly().
from_json(_Json) ->
    throw(?LINE).

-spec to_binary(Assembly::assembly()) -> binary().
to_binary(_Assembly) ->
    throw(?LINE).

-spec from_binary(Binary::binary()) -> assembly().
from_binary(_Binary) ->
    throw(?LINE).

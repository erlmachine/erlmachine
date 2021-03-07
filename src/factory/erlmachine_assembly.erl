-module(erlmachine_assembly).

-behaviour(erlmachine_db).

%% API.

-export([new/0, new/1, new/5]).

-export([
         id/1, id/2,
         serial_no/1, serial_no/2,
         type/1, type/2,
         body/1, body/2,
         model_no/1, model_no/2,
         socket/1, socket/2,
         graph/1, graph/2,
         model/1, model/2,
         prototype/1, prototype/2,
         extensions/1, extensions/2,
         uid/1, uid/2,
         tags/1, tags/2,
         vertex/1, vertex/2,
         part_no/1, part_no/2,
         env/1, env/2,
         description/1, description/2
        ]).

-export([datasheet/2]).

-include("erlmachine_user.hrl").
-include("erlmachine_factory.hrl").
-include("erlmachine_system.hrl").

%% NOTE: Here is implemented incapsulation across independent parts and the whole transmission too;
%% We consider Name as implementation point (like class) and serial number as instance - (like object);
%% We can support polymorphism by different ways - by overriding prototype or by changing model itself;

%% NOTE: I am thinking about two kinds of assembly manual and automated;
%% Manual issued by canvas whereas automated via code

-record (assembly, {
                    %% Produced number which is assigned by factory
                    id::integer(),
                    %% Runtime unique identifier (S/N)
                    serial_no::serial_no(),
                    %% Behavioral type of an extension
                    type::type(),
                    %% Data structure that stores the state of an extension
                    body::term(),
                    %% Product configurator input to generate a master production schedule
                    model_no::model_no(),
                    %% Interface (shape) of an extension
                    socket::term(),
                    %% Build topology which is inherited through the all extensions
                    graph::graph(),
                    %% Domain level specification
                    model::model(),
                    %% Service level specification
                    prototype::prototype(),
                    %% Build configuration
                    extensions = []::[assembly()],
                    %% The identity of Erlmachine operator
                    uid::uid(),
                    %% Index terms which are assigned as meta-information
                    tags = []::[term()],
                    %% The identity on a graph (by default serial_no)
                    vertex::vertex(),
                    %% Deployment identity to track the quality of a component through release period
                    part_no::part_no(),
                    %% The environment context which inherites from the root
                    env::map(),
                    %% Short overview of the extension role
                    description::binary()
                   }
        ).

-type type() :: 'worker' | 'supervisor'.

-type graph() :: erlmachine_graph:graph().

-type model() :: erlmachine_model:model().
-type prototype() :: erlmachine_prototype:prototype().

-type datasheet() :: erlmachine_datasheet:datasheet().

-type opaque() :: #assembly{}.

-export_type([assembly/0]).

%%% Constructor

-spec new() -> assembly().
new() ->
    #assembly{}.

-spec new(ModelName, ModelOpt, ProtName, ProtOpt, Tags) ->
                      assembly().
new(Module, Socket, Body, Tags, Desc) ->
    Assembly = new(Socket),
    Rel = module(body(Assembly, Body), Module), description(tags(Rel, Tags), Desc).

%%% Datasheet processing

-spec datasheet(Assembly::assembly(), Datasheet::datasheet()) -> assembly().
datasheet(Assembly, Datasheet) ->
    I = erlmachine_datasheet:iterator(Datasheet), Next = erlmachine_datasheet:next(I),
    next(Assembly, Next).

-spec next(Assembly::assembly(), none | {Key::binary(), Value::term(), I::term()}) ->
                  assembly().
next(Assembly, none) ->
    Assembly;

next(Assembly, {<<"serial_no">>, V, I}) ->
    Rel = serial_no(Assembly, V),
    next(Rel, erlmachine_datasheet:next(I));

next(Assembly, {<<"type">>, V, I}) ->
    Rel = type(Assembly, binary_to_atom(V, utf8)),
    next(Rel, erlmachine_datasheet:next(I));

next(Assembly, {<<"body">>, V, I}) ->
    Rel = body(Assembly, V),
    next(Rel, erlmachine_datasheet:next(I));

next(Assembly, {<<"model_no">>, V, I}) ->
    Rel = model_no(Assembly, V),
    next(Rel, erlmachine_datasheet:next(I));

next(Assembly, {<<"socket">>, V, I}) ->
    Rel = socket(Assembly, V),
    next(Rel, erlmachine_datasheet:next(I));

next(Assembly, {<<"model">>, V, I}) ->
    {ok, Name} = erlmachine_datasheet:find(<<"module">>, V), Module = binary_to_existing_atom(Name, utf8),
    {ok, Opt} = erlmachine_datasheet:find(<<"options">>, V),
    Model =
        case erlmachine_datasheet:find(<<"vsn">>, V) of
            {ok, Vsn} ->
                erlmachine_model:new(Module, Opt, Vsn);
            _ ->
                erlmachine_model:new(Module, Opt)
        end,
    Rel = model(Assembly, Model),
    next(Rel, erlmachine_datasheet:next(I));

next(Assembly, {<<"prototype">>, V, I}) ->
    {ok, Name} = erlmachine_datasheet:find(<<"module">>, V), Module = binary_to_existing_atom(Name, utf8),
    {ok, Opt} = erlmachine_datasheet:find(<<"options">>, V),
    Prot =
        case erlmachine_datasheet:find(<<"vsn">>, V) of
            {ok, Vsn} ->
                erlmachine_prototype:new(Module, Opt, Vsn);
            _ ->
                erlmachine_prototype:new(Module, Opt)
        end,
    Rel = prototype(Assembly, Prot),
    next(Rel, erlmachine_datasheet:next(I));

next(Assembly, {<<"uid">>, V, I}) ->
    Rel = uid(Assembly, V),
    next(Rel, erlmachine_datasheet:next(I));

next(Assembly, {<<"tags">>, V, I}) ->
    Rel = tags(Assembly, V),
    next(Rel, erlmachine_datasheet:next(I));

next(Assembly, {<<"vertex">>, V, I}) ->
    Rel = vertex(Assembly, V),
    next(Rel, erlmachine_datasheet:next(I));

next(Assembly, {<<"part_no">>, V, I}) ->
    Rel = part_no(Assembly, V),
    next(Rel, erlmachine_datasheet:next(I));

next(Assembly, {<<"env">>, V, I}) ->
    Rel = env(Assembly, V),
    next(Rel, erlmachine_datasheet:next(I));

next(Assembly, {<<"description">>, V, I}) ->
    Rel = description(Assembly, V),
    next(Rel, erlmachine_datasheet:next(I));

next(Assembly, {_, _, I}) ->
    next(Assembly, erlmachine_datasheet:next(I)).

%%% Field accessors

-spec id(Assembly::assembly()) -> integer().
id(Assembly) ->
    Assembly#assembly.id.

-spec id(Assembly::assembly(), ID::integer()) -> assembly().
id(Assembly, ID) ->
    Assembly#assembly{ id = ID }.

-spec serial_no(Assembly::assembly()) -> serial_no().
serial_no(Assembly) ->
    Assembly#assembly.serial_no.

-spec serial_no(Assembly::assembly(), SN::serial_no()) -> assembly().
serial_no(Assembly, SN) ->
    Assembly#assembly{ serial_no = SN }.

-spec type(Assembly::assembly()) -> type().
type(Assembly) ->
    Assembly#assembly.type.

-spec type(Assembly::assembly(), Type::type()) -> assembly().
type(Assembly, Type) ->
    Assembly#assembly{ type = Type }.

-spec body(Assembly::assembly()) -> term().
body(Assembly) ->
    Assembly#assembly.body.

-spec body(Assembly::assembly(), Body::term()) -> assembly().
body(Assembly, Body) ->
    Assembly#assembly{ body = Body }.

-spec model_no(Assembly::assembly()) -> model_no().
model_no(Assembly) ->
    Assembly#assembly.model_no.

-spec model_no(Assembly::assembly(), MN::model_no()) -> assembly().
model_no(Assembly, MN) ->
    Assembly#assembly{ model_no = MN }.

-spec socket(Assembly::assembly()) -> term().
socket(Assembly) -> 
    Assembly#assembly.socket.

-spec socket(Assembly::assembly(), Socket::term()) -> assembly().
socket(Assembly, Socket) ->
    Assembly#assembly{ socket = Socket }.

-spec graph(Assembly::assembly()) -> graph().
graph(Assembly) ->
    Assembly#assembly.graph.

-spec graph(Assembly::assembly(), Graph::graph()) -> assembly().
graph(Assembly, Graph) ->
    Assembly#assembly{ graph = Graph }.

-spec model(Assembly::assembly()) -> model().
model(Assembly) ->
    Assembly#assembly.model.

-spec model(Assembly::assembly(), Model::model()) -> assembly().
model(Assembly, Model) ->
    Assembly#assembly{ model = Model }.

-spec prototype(Assembly::assembly()) -> prototype().
prototype(Assembly) ->
    Assembly#assembly.prototype.

-spec prototype(Assembly::assembly(), Prot::prototype()) -> assembly().
prototype(Assembly, Prot) ->
    Assembly#assembly{ prototype = Prot }.

-spec extensions(Assembly::assembly()) -> [assembly()].
extensions(Assembly) ->
    Assembly#assembly.extensions.

-spec extensions(Assembly::assembly(), Exts::[assembly()]) -> assembly().
extensions(Assembly, Exts) ->
    Assembly#assembly{ extensions = Exts }.

-spec uid(Assembly::assembly()) -> uid().
uid(Assembly) ->
    Assembly#assembly.uid.

-spec uid(Assembly::assembly(), UID::uid()) -> assembly().
uid(Assembly, UID) ->
    Assembly#assembly{ uid = UID }.

-spec part_no(Assembly::assembly()) -> term().
part_no(Assembly) ->
    Assembly#assembly.part_no.

-spec part_no(Assembly::assembly(), PN::term()) -> assembly().
part_no(Assembly, PN) ->
    Assembly#assembly{ part_no = PN }.

-spec tags(Assembly::assembly()) -> term().
tags(Assembly) ->
    Assembly#assembly.tags.

-spec tags(Assembly::assembly(), Tags::term()) -> assembly().
tags(Assembly, Tags) ->
    Assembly#assembly{ tags = Tags }.

-spec vertex(Assembly::assembly()) -> term().
vertex(Assembly) ->
    Assembly#assembly.vertex.

-spec vertex(Assembly::assembly(), Vertex::term()) -> assembly().
vertex(Assembly, Vertex) ->
    Assembly#assembly{ vertex = Vertex }.

-spec env(Assembly::assembly()) -> term().
env(Assembly) ->
    Assembly#assembly.env.

-spec env(Assembly::assembly(), Env::term()) -> assembly().
env(Assembly, Env) ->
    Assembly#assembly{ env = Env }.

-spec description(Assembly::assembly()) -> binary().
description(Assembly) ->
    Assembly#assembly.description.

-spec description(Assembly::assembly(), Desc::binary()) -> assembly().
description(Assembly, Desc) ->
    Assembly#assembly{ description = Desc }.

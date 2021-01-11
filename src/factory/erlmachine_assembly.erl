-module(erlmachine_assembly).

%% API.

-export([assembly/1, assembly/5]).

-export([
         serial_no/1, serial_no/2,
         name/1, name/2,
         body/1, body/2,
         model_no/1, model_no/2,
         socket/1, socket/2,
         schema/1, schema/2,
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

-export([store/2, delete/2, find/2]).

-include("erlmachine_user.hrl").
-include("erlmachine_factory.hrl").
-include("erlmachine_system.hrl").

%% NOTE: Here is implemented incapsulation across independent parts and the whole transmission too;
%% We consider Name as implementation point (like class) and serial number as instance - (like object);
%% We can support polymorphism by different ways - by overriding prototype or by changing model itself;

%% NOTE: I am thinking about two kinds of assembly manual and automated;

%% a) Manual applies through the canvas;
%% b) The assembly itself and any potential future changes should be persisted;

-record (assembly, {
                    %% Runtime unique identifier (S/N)
                    %% TODO: Don't forget to pass it as args list to the supervisor model
                    %% That is the right approach for supervsior (to have knowledge about runtime ID's)
                    serial_no::serial_no(),
                    %% Extension module: erlmachine_axle, erlmachine_shaft, erlmachine_gearbox, erlmachine_gear
                    name::atom(),
                    %% Body that stores the current state
                    body::term(),
                    %% A model_no can be used by product configurator to generate a master production schedule
                    model_no::model_no(),
                    %% Interface which is passed into the rotate call)
                    socket::term(),
                    %% Build topology which is inherited through the all extensions
                    schema::schema(),
                    %% Domain level specification
                    model::model(),
                    %% Service level specification
                    prototype::prototype(),
                    %% Build configuration
                    extensions = []::[assembly()],
                    %% Machine operator
                    uid::uid(),
                    %% Tags are used as selection criteria ([supervisor, overloaded, etc.])
                    tags = []::list(term()),
                    %% An alternative id within schema (by default serial_no)
                    vertex::term(),
                    %% By part_no we can track quality of component through release period
                    part_no::part_no(),
                    %% The execution context which is inherited through the extensions
                    env::term(),
                    %% Textual description of the extension
                    description::binary()
                   }
        ).

-type schema() :: erlmachine_schema:schema().

-type model() :: erlmachine_model:model().
-type prototype() :: erlmachine_prototype:prototype().

-type assembly() :: #assembly{}.

-export_type([assembly/0]).

-spec assembly(Socket::term()) -> assembly().
assembly(Socket) ->
    #assembly{ socket = Socket }.

-spec assembly(Name::atom(), Socket::term(), Body::term(), Tags::list(), Desc::binary()) -> 
                      assembly().
assembly(Name, Socket, Body, Tags, Desc) ->
    Rel = name(body(assembly(Socket), Body), Name),
    description(tags(Rel, Tags), Desc).

-spec serial_no(Assembly::assembly()) -> serial_no().
serial_no(Assembly) ->
    SN = Assembly#assembly.serial_no,
    SN.

-spec serial_no(Assembly::assembly(), SN::serial_no()) -> assembly().
serial_no(Assembly, SN) ->
    Assembly#assembly{ serial_no=SN }.

-spec name(Assembly::assembly()) -> atom().
name(Assembly) ->
    Assembly#assembly.name.

-spec name(Assembly::assembly(), Name::atom()) -> assembly().
name(Assembly, Name) ->
    Assembly#assembly{ name=Name }.

-spec body(Assembly::assembly()) -> term().
body(Assembly) ->
    Assembly#assembly.body.

-spec body(Assembly::assembly(), Body::term()) -> assembly().
body(Assembly, Body) ->
    Assembly#assembly{ body=Body }.

-spec model_no(Assembly::assembly()) -> model_no().
model_no(Assembly) ->
    Assembly#assembly.model_no.

-spec model_no(Assembly::assembly(), MN::model_no()) -> assembly().
model_no(Assembly, MN) ->
    Assembly#assembly{ model_no=MN }.

-spec socket(Assembly::assembly()) -> term().
socket(Assembly) -> 
    Assembly#assembly.socket.

-spec socket(Assembly::assembly(), Socket::term()) -> assembly().
socket(Assembly, Socket) ->
    Assembly#assembly{ socket = Socket }.

-spec schema(Assembly::assembly()) -> term().
schema(Assembly) ->
    Assembly#assembly.schema.

-spec schema(Assembly::assembly(), Schema::term()) -> assembly().
schema(Assembly, Schema) ->
    Assembly#assembly{ schema=Schema }.

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
    Assembly#assembly{ part_no=PN }.

-spec tags(Assembly::assembly()) -> term().
tags(Assembly) ->
    Assembly#assembly.tags.

-spec tags(Assembly::assembly(), Tags::term()) -> assembly().
tags(Assembly, Tags) ->
    Assembly#assembly{ tags=Tags }.

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

-spec store(Assembly::assembly(), Ext::assembly()) -> assembly().
store(Assembly, Ext) ->
    Exts = lists:keystore(vertex(Ext), #assembly.vertex, extensions(Assembly), Ext),
    extensions(Assembly, Exts).

-spec delete(Assembly::assembly(), Vertex::term()) -> assembly().
delete(Assembly, Vertex) ->
    Exts = lists:keydelete(Vertex, #assembly.vertex, extensions(Assembly)),
    extensions(Assembly, Exts).

-spec find(Assembly::assembly(), Vertex::term()) -> assembly().
find(Assembly, Vertex) ->
    lists:keyfind(Vertex, #assembly.vertex, extensions(Assembly)).

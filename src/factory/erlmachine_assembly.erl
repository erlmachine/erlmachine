-module(erlmachine_assembly).

%% API.

-export([assembly/0, assembly/4]).

-export([
         serial_no/1, serial_no/2,
         name/1, name/2,
         body/1, body/2,
         socket/1, socket/2,
         schema/1, schema/2,
         model/1, model/2,
         extensions/1, extensions/2,
         tags/1, tags/2,
         label/1, label/2,
         part_no/1, part_no/2,
         env/1, env/2,
         desc/1, desc/2
        ]).

-export([store/2, delete/2, find/2]).

-include("erlmachine_factory.hrl").
-include("erlmachine_system.hrl").

%% NOTE: Here is implemented incapsulation across independent parts and the whole transmission too;
%% We consider Name as implementation point (like class) and serial number as instance - (like object);
%% We can support polymorphism by different ways - by overriding prototype or by changing model itself;

%% NOTE: I am thinking about two kinds of assembly manual and automated;

%% a) Manual applies through the canvas;
%% b) The assembly itself and any potential future changes should be persisted;

-record (assembly, {
                    %% Runtime unique identifier (S/N); 
                    %% TODO: Don't forget to pass it as args list to the supervisor model;
                    %% That is the right approach for supervsior (to have knowledge about runtime ID's);
                    serial_no::serial_no(),
                    %% Extension module: erlmachine_axle, erlmachine_shaft, erlmachine_gearbox, erlmachine_gear;
                    name::atom(),
                    %% Body that stores the current state;
                    body::term(),
                    %% Interface which is passed into the rotate call);
                    socket::term(),
                    %% The build topology which is inherited through the all extensions;
                    schema::term(),
                    %% The assembly setup;
                    model::model(),
                    %% Build configuration;
                    extensions=[]::list(assembly()),
                    %% Tags are used as selection criteria ([supervisor, overloaded, etc.]);
                    tags=[]::list(term()),
                    %% Label is unique id within schema (by default serial_no);
                    label::term(),
                    %% By part_no we can track quality of component through release period;
                    part_no::term(),
                    %% The execution context which is inherited through the extensions;
                    env::term(),
                    %% Textual description of the extension;
                    desc::binary()
                   }
        ).

-type assembly() :: #assembly{}.

-type model() :: erlmachine_model:model().

-export_type([assembly/0]).

-spec assembly() -> assembly().
assembly() ->
    #assembly{}.

-spec assembly(Name::atom(), Body::term(), Tags::list(), Desc::binary()) -> 
                      assembly().
assembly(Name, Body, Tags, Desc) ->
    Rel = name(body(assembly(), Body), Name),
    desc(tags(Rel, Tags), Desc).

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

-spec extensions(Assembly::assembly()) -> list(assembly()).
extensions(Assembly) ->
    Assembly#assembly.extensions.

-spec extensions(Assembly::assembly(), Exts::list(assembly())) -> assembly().
extensions(Assembly, Exts) ->
    Assembly#assembly{ extensions=Exts }.

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

-spec label(Assembly::assembly()) -> term().
label(Assembly) ->
    Assembly#assembly.label.

-spec label(Assembly::assembly(), Label::term()) -> assembly().
label(Assembly, Label) ->
    Assembly#assembly{ label = Label }.

-spec env(Assembly::assembly()) -> term().
env(Assembly) ->
    Assembly#assembly.env.

-spec env(Assembly::assembly(), Env::term()) -> assembly().
env(Assembly, Env) ->
    Assembly#assembly{ env = Env }.

-spec desc(Assembly::assembly()) -> binary().
desc(Assembly) ->
    Assembly#assembly.desc.

-spec desc(Assembly::assembly(), Desc::binary()) -> assembly().
desc(Assembly, Desc) ->
    Assembly#assembly{ desc = Desc }.

-spec store(Assembly::assembly(), Ext::assembly()) -> assembly().
store(Assembly, Ext) ->
    Exts = lists:keystore(label(Ext), #assembly.label, extensions(Assembly), Ext),
    extensions(Assembly, Exts).

-spec delete(Assembly::assembly(), Label::term()) -> assembly().
delete(Assembly, Label) ->
    Exts = lists:keydelete(Label, #assembly.label, extensions(Assembly)),
    extensions(Assembly, Exts).

-spec find(Assembly::assembly(), Label::term()) -> assembly().
find(Assembly, Label) ->
    lists:keyfind(Label, #assembly.label, extensions(Assembly)).

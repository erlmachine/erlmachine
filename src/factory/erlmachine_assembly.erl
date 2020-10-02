-module(erlmachine_assembly).

%% API.

-export([assembly/3, assembly/4]).

-export([install/1, install/2]).
-export([uninstall/1, uninstall/2]).

-export([
         serial_no/1, serial_no/2,
         name/1, name/2,
         fixed/1, fixed/2,
         body/1, body/2,
         socket/1, socket/2,
         schema/1, schema/2,
         model/1, model/2,
         extensions/1, extensions/2,
         tags/1, tags/2,
         label/0, label/1, label/2,
         part_no/1, part_no/2,
         env/1, env/2,
         desc/1, desc/2
        ]).

-export([store/2, delete/2, find/2]).

-export([tag/2, untag/2]).

-include("erlmachine_system.hrl").

%% Here is implemented incapsulation across independent parts and the whole transmission too;
%% We consider Name as implementation point (like class) and serial number as instance - (like object);
%% We can support polymorphism by different ways - by overriding prototype or by changing model itself;

-type model() :: erlmachine_factory:model().

%% I am thinking about two kinds of assembly manual and automated;

%% a) Manual applies through the canvas;
%% b) The assembly itself and any potential future changes should be persisted;

-record (assembly, {
                    serial_no::serial_no(),
                    %% Name contains: erlmachine_axle, erlmachine_shaft, erlmachine_gearbox, erlmachine_gear;
                    name::atom(),
                    fixed::serial_no(),
                    %% Body stores the current state of an extension;
                    body::term(),
                    %% Connection interface (is passed to the rotate call);
                    socket::term(),
                    %% The build topology of the current gearbox (is inherited through the all extensions);
                    schema::term(),
                    %% The assembly setup;
                    model::model(),
                    %% Build configuration;
                    extensions=[]::list(assembly()),
                    %% Tags can be used as selection criteria ([supervisor, overloaded, etc.]);
                    tags=[]::list(term()),
                    %% Label is unique access id within gearbox (serial_no by default);
                    label::term(),
                    %% By part_no we can track quality of component through release period, etc.;
                    part_no::term(),
                    %% The defined context of the current gearbox (is inherited through the all extensions);
                    env::term(),
                    %% Textual description of the component;
                    desc::binary()
                   }
        ).

-type assembly() :: #assembly{}.

-export_type([assembly/0]).

-spec assembly() -> assembly().
assembly() ->
    #assembly{}.

-spec assembly(Name::atom(), Body::term(), Model::model()) -> 
                      assembly().
assembly(Name, Body, Model) ->
    Assembly = body(assembly(), Body),
    name(model(Assembly, Model), Name).

-spec assembly(Name::atom(), Schema::term(), Body::term(), Model::model(), Env::term()) -> 
                      assembly().
assembly(Name, Schema, Body, Model, Env) ->
    Assembly = assembly(Name, Body, Model),
    schema(env(Assembly, Env), Schema).

%% TODO: To operate only via schema;
%% Schema has to be extracted and operated via independent way (to be able to manage gearbox you have to store it);
-spec install(Assembly::assembly()) ->
                     success(pid()) | failure(term(), term()).
install(Assembly) ->
    Name = name(Assembly),
    try 
        {ok, Pid} = Name:install(Assembly), true = is_pid(Pid),
        %% Build gearbox schema;
        init(Assembly),
        erlmachine:success(Pid)
    catch E:R ->
            erlmachine:failure(E, R)
    end.

-spec install(Assembly::assembly(), Ext::assembly()) ->
                    success(pid()) | failure(term(), term()).
install(Assembly, Ext) ->
    Name = name(Assembly), Label = label(Assembly),
    try
        {ok, Pid} = Name:install(Assembly, Ext), true = is_pid(Pid),
        %% Build edge (Assembly -> Ext);
        %% Each ext stores schema within;
        erlmachine_schema:add_edge(Assembly, Label, Ext),
        erlmachine:success(Pid)
    catch E:R ->
            erlmachine:failure(E, R)
    end.

-spec uninstall(Assembly::assembly(), Id::term()) -> 
                       success(term()) | failure(term(), term()).
uninstall(Assembly, Id) ->
    SN = serial_no(Assembly),
    Res = (prototype_name(Assembly)):detach(SN, Assembly, Id),
    %% Remove vertex with all edges (GearBox -> Part);
    ok = erlmachine_schema:del_vertex(Assembly, Id),
    Res.

-spec uninstall(Assembly::assembly()) ->
                       success().
uninstall(Assembly) ->
    Name = name(Assembly), Label = label(Assembly),
    Res = Name:uninstall(Assembly), ok = Res,
    ok = erlmachine_schema:del_vertex(Assembly, Label),
    erlmachine:success().

-spec init(Assembly::assembly()) -> success().
init(Assembly) ->
    Schema = erlmachine_assembly:schema(Assembly),

    V = erlmachine_schema:add_vertex(Schema, Assembly),
    init(Schema, V, erlmachine_assembly:parts(Assembly)), 
    ok.

-spec init(Schema::term(), V1::vertex(), Parts::list(assembly())) -> 
                  vertex().
init(_Schema, V1, []) ->
    V1;
init(Schema, V1, [Part|T]) ->
    V2 = add_edge(Schema, V1, Part),

    init(Schema, V2, erlmachine_assembly:parts(Part)),
    init(Schema, V1, T),
    V1.

%% Accessors;
-spec serial_no() -> integer().
serial_no() ->
    #assembly.serial_no.

-spec serial_no(Assembly::assembly()) -> serial_no().
serial_no(Assembly) ->
    SN = Assembly#assembly.serial_no,
    SN.

-spec serial_no(Assembly::assembly(), SN::serial_no()) -> Release::assembly().
serial_no(Assembly, SN) ->
    Assembly#assembly{ serial_no=SN }.

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
schema(Assembly, Body) ->
    Assembly#assembly{ schema=Schema }.

-spec model(Assembly::assembly()) -> model().
model(Assembly) ->
    Assembly#assembly.model.

-spec model(Assembly::assembly(), Model::model()) -> assembly().
model(Assembly, Model) ->
    Assembly#assembly{ model = Model }.

-spec prototype(Assembly::assembly()) -> prototype().
prototype(Assembly) ->
    Model = model(Assembly),
    erlmachine_model:prototype(Model).

-spec prototype(Assembly::assembly(), Prototype::prototype()) -> assembly().
prototype(Assembly, Prototype) ->
    Model = model(Assembly),
    model(Assembly, erlmachine_model:prototype(Model, Prototype)).

-spec extensions(Assembly::assembly()) -> list(assembly()).
extensions(Assembly) ->
    Assembly#assembly.extensions.

-spec extensions(Assembly::assembly(), Exts::list(assembly())) -> assembly().
extensions(Assembly, Exts) ->
    Assembly#assembly{ extensions=Exts }.

-spec fixed(Assembly::assembly()) -> assembly().
fixed(Assembly) ->
    Assembly#assembly.fixed.

-spec fixed(Assembly::assembly(), SN::serial_no()) -> assembly().
fixed(Assembly, SN) ->
    Assembly#assembly{ fixed=SN }.

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

-spec label(Assembly::assembly()) -> label().
label(Assembly) ->
    Assembly#assembly.label.

-spec label(Assembly::assembly(), Label::label()) -> assembly().
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

%% Misc
-spec store(Assembly::assembly(), Ext::assembly()) -> assembly().
store(Assembly, Ext) ->
    Exts = lists:keystore(label(Extension), label(), extensions(Assembly), Ext),
    extensions(Assembly, Exts).

-spec delete(Assembly::assembly(), Label::term()) -> assembly().
delete(Assembly, Label) ->
    Exts = lists:keydelete(Label, label(), extensions(Assembly)),
    extensions(Assembly, Exts).

-spec find(Assembly::assembly(), Label::term()) -> assembly().
find(Assembly, Label) ->
    lists:keyfind(Label, label(), extensions(Assembly)).

-spec tag(Assembly::assembly(), Tag::term()) -> assembly().
tag(Assembly, Tag) ->
    Tags = tags(Assembly),
    tags(Assembly, [Tag|Tags]).

-spec untag(Assembly::assembly(), Tag::term()) -> assembly().
untag(Assembly, Tag) ->
    Tags = tags(Assembly),
    tags(Assembly, lists:delete(Tag, Tags)).

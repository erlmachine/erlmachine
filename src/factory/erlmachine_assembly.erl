-module(erlmachine_assembly).

%% API.

-export([assembly/4]).

-export([model/0, prototype/0]).

-export([
         install/1, install/2,
         attach/3, attach/4,
         detach/2, detach/3,
         uninstall/2, uninstall/3
        ]).

-export([
         installed/2, 
         attached/3, detached/3,
         replaced/3,
         overloaded/3, blocked/4, %% TODO relocate to erlmachine_system
         uninstalled/3
        ]).

-export([
         serial_no/0, serial_no/1, serial_no/2,

         body/1, body/2,

         schema/0, schema/1, schema/2,

         model/1, model/2, 
         product/1, product/2,
         prototype/1, prototype/2, 

         model_name/1, model_name/2,
         model_options/1, model_options/2,
         model_no/1, model_no/2,
         model_digest/1, model_digest/2,

         prototype_name/1, prototype_name/2,
         prototype_body/1, prototype_body/2,
         prototype_options/1, prototype_options/2,

         parts/1, parts/2,
         mounted/1, mounted/2,
         is_mounted/1,

         part_no/1, part_no/2,

         tags/1, tags/2,
         label/1, label/2
        ]).

-export([add/2, remove/2, part/2]).%% TODO can be relocated;

-export([tabname/0, record_name/0, attributes/0]).

%% CRUD
-export([create/1, read/1, update/1, delete/1]).

-include("erlmachine_system.hrl").

%% The main purpose of this module is to instantiate proceses accordingly to design file;
%% In this module will be provided incapsulation around building of independent parts and whole transmission too;
%% We consider Module as implementation point (like class) and serial number as instance - (like object); 
%% We can support polymorphism by different ways - by overriding prototype or by changing topology itself;

-type serial_no() :: erlmachine_factory:serial_no().

-type model_no() :: erlmachine_factory:model_no().
-type part_no() :: erlmachine_factory:part_no().

-type model() :: erlmachine_factory:model().
-type prototype() :: erlmachine_factory:prototype().
-type product() :: erlmachine_factory:product().

%% I am thinking about two kinds of assembly manual and automated;
%% The main difference between them is manual needs to be stored with body, and any changes need to be persisted;
%% Automated exists in code but manual doesn't;

%% The model is a key, all other parts and assembly itself can be restored;
-record (assembly, {
                    %% We can get build info (ts, etc..) by serial number from db;
                    serial_no::serial_no(),
                    body::term(),
                    schema::term(),
                    model::model() | model_no(),
                    mounted::assembly() | serial_no(),
                    parts=[]::list(assembly() | serial_no()),
                    tags=[]::list(term()),
                    label::label(),
                    %% By part_no we can be able to track quality of component through release period;
                    part_no::part_no()
                   }
        ).

-type assembly() :: #assembly{}.

-type label() :: term().

-export_type([assembly/0]).

-spec install(GearBox::assembly()) ->
                     success(pid()) | ingnore | failure(term()).
install(GearBox) ->
    SN = serial_no(GearBox),
    Opt = prototype_options(GearBox),
    %% TODO at that place we can register information about scheme in persistence storage;
    Res = (prototype_name(GearBox)):install(SN, GearBox, Opt), 
    %% Build whole schema;
    Schema = schema(GearBox),
    erlmachine_schema:add_edges(Schema, GearBox),
    Res.

-spec install(GearBox::assembly(), Part::assembly()) ->
                     success(pid()) | ingnore | failure(E::term()).
install(GearBox, Part) ->
    SN = serial_no(Part),
    Opt = prototype_options(Part),
    Res = (prototype_name(Part)):install(SN, GearBox, Part, Opt),
    %% Build part schema;
    Schema = schema(GearBox),
    erlmachine_schema:add_edges(Schema, Part),
    Res.

-spec attach(GearBox::assembly(), Reg::term(), Ext::assembly()) -> 
                    success(term(), term()) | failure(term(), term()).
attach(GearBox, Reg, Ext) ->
    SN = serial_no(GearBox),
    Res = (prototype_name(GearBox)):attach(SN, GearBox, Reg, Ext),
    %% Build edge (GearBox -> Part);
    erlmachine_schema:add_edge(schema(GearBox), label(GearBox), Ext), 
    Res.

-spec attach(GearBox::assembly(), Label::term(), Reg::term(), Ext::assembly()) -> 
                    success(term(), term()) | failure(term(), term()).
attach(GearBox, Label, Reg, Ext) ->
    Part = erlmachine_gearbox:find(GearBox, Label),
    SN = serial_no(Part),
    Res = (prototype_name(Part)):attach(SN, GearBox, Part, Reg, Ext),
    %% Build edge (Part -> Ext);
    erlmachine_schema:add_edge(schema(GearBox), Label, Ext),
    Res.

-spec detach(GearBox::assembly(), ID::serial_no()) -> 
                    success(term()) | failure(term(), term()).
detach(GearBox, ID) ->
    SN = serial_no(GearBox),
    Res = (prototype_name(GearBox)):detach(SN, GearBox, ID),
    %% Remove vertex with all edges (GearBox -> Part);
    ok = erlmachine_schema:del_vertex(schema(GearBox), ID),
    Res.

-spec detach(GearBox::assembly(), Label::term(), ID::term()) -> 
                    success(term()) | failure(term(), term()).
detach(GearBox, Label, ID) ->
    Part = erlmachine_gearbox:find(GearBox, Label),
    SN = serial_no(Part),
    Res = (prototype_name(Part)):detach(SN, GearBox, Part, ID),
    %% Remove edge (Part -> Ext);
    ok = erlmachine_schema:del_path(schema(GearBox), Label, ID),
    Res.

-spec uninstall(GearBox::assembly(), Reason::term()) ->
                     ok.
uninstall(GearBox, Reason) ->
    SN = serial_no(GearBox),
    Res = (prototype_name(GearBox)):uninstall(SN, GearBox, Reason),
    %% Remove vertex with all edges (GearBox);
    ok = erlmachine_schema:del_vertex(schema(GearBox), label(GearBox)),
    Res.

-spec uninstall(GearBox::assembly(), Label::term(), Reason::term()) ->
                       ok.
uninstall(GearBox, Label, Reason) ->
    Part = erlmachine_gearbox:find(GearBox, Label),
    SN = serial_no(Part),
    Res = (prototype_name(Part)):uninstall(SN, GearBox, Part, Reason),
    %% Remove vertex with all edges (Part);
    ok = erlmachine_schema:del_vertex(schema(GearBox), Label),
    Res.

-spec installed(GearBox::assembly(), Assembly::assembly()) -> 
                         ok.
installed(GearBox, Assembly) ->
    Mounted = mounted(Assembly),
    SN0 = serial_no(Mounted), SN1 = serial_no(GearBox),
    (prototype_name(GearBox)):installed(SN1, GearBox, Assembly),
    (SN1 == SN0) orelse 
                   (prototype_name(Mounted)):installed(SN0, GearBox, Mounted, Assembly),
    %% NOTE Instead of access from external process we are going to provide
    %% notification and update monitoring copy with suitable tags, etc.;
    ok.

-spec attached(GearBox::assembly(), Part::assembly(), Extension::assembly()) -> 
                       ok.
attached(GearBox, Part, Extension) ->
    SN = serial_no(GearBox),
    (prototype_name(GearBox)):attached(SN, GearBox, Part, Extension).

-spec detached(GearBox::assembly(), Part::assembly(), ID::serial_no()) -> 
                      ok.
detached(GearBox, Part, ID) ->
    SN = serial_no(GearBox),
    (prototype_name(GearBox)):detached(SN, GearBox, Part, ID).

-spec replaced(GearBox::assembly(), Assembly::assembly(), Part::assembly()) -> 
                      ok.
replaced(GearBox, Assembly, Part) ->
    SN = serial_no(GearBox),
    (prototype_name(GearBox)):replaced(SN, GearBox, Assembly, Part).

-spec overloaded(GearBox::assembly(), Assembly::assembly(), Load::term()) -> 
                      ok.
overloaded(GearBox, Assembly, Load) ->
    SN = serial_no(GearBox),
    (prototype_name(GearBox)):overloaded(SN, GearBox, Assembly, Load).

-spec blocked(GearBox::assembly(), Part::assembly(), Ext::assembly(), Failure::term()) -> 
                        ok.
blocked(GearBox, Part, Ext, Failure) ->
    SN = serial_no(GearBox),
    (prototype_name(GearBox)):blocked(SN, GearBox, Part, Ext, Failure).

-spec uninstalled(GearBox::assembly(), Part::assembly(), Reason::term()) -> 
                       ok.
uninstalled(GearBox, Part, Reason) ->
    Mounted = mounted(Part),
    SN0 = serial_no(Mounted), SN1 = serial_no(GearBox),
    (prototype_name(GearBox)):uninstalled(SN1, GearBox, Part, Reason),
    (SN1 == SN0) orelse 
                   (prototype_name(Mounted)):uninstalled(SN0, GearBox, Mounted, Part, Reason),
    %% NOTE Instead of access from external process we are going to provide
    %% notification and update monitoring copy with suitable tags, etc.;
    ok.

-spec assembly(SN::serial_no(), Body::term(), Model::model(), Label::term()) -> assembly().
assembly(SN, Body, Model, Label) ->
    #assembly{ serial_no=SN, body=Body, model=Model, label=Label }.

-spec is_mounted(Assembly::assembly()) -> boolean().
is_mounted(Assembly) -> 
    mounted(Assembly) /= undefined.

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

-spec schema() -> term().
schema() ->
    digraph:new().

-spec schema(Assembly::assembly()) -> term().
schema(Assembly) ->
    Assembly#assembly.schema.

-spec schema(Assembly::assembly(), Schema::term()) -> assembly().
schema(Assembly, Schema) ->
    Assembly#assembly{ schema=Schema }.

-spec model() -> model().
model() ->
    erlmachine_model:model().

-spec model(Assembly::assembly()) -> model().
model(Assembly) ->
    Assembly#assembly.model.

-spec model(Assembly::assembly(), Model::model()) -> assembly().
model(Assembly, Model) ->
    Assembly#assembly{model = Model}.

-spec prototype(Assembly::assembly()) -> prototype().
prototype(Assembly) ->
    Model = model(Assembly),
    erlmachine_model:prototype(Model).

-spec prototype(Assembly::assembly(), Prototype::prototype()) -> assembly().
prototype(Assembly, Prototype) ->
    Model = model(Assembly),
    model(Assembly, erlmachine_model:prototype(Model, Prototype)).

-spec model_no(Assembly::assembly()) -> MN::term().
model_no(Assembly) ->
    Model = model(Assembly),
    erlmachine_model:model_no(Model).

-spec model_no(Assembly::assembly(), MN::term()) -> assembly().
model_no(Assembly, MN) ->
    Model = model(Assembly),
    model(Assembly, erlmachine_model:model_no(Model, MN)).

-spec model_name(Assembly::assembly()) -> atom().
model_name(Assembly) ->
    Model = model(Assembly),
    erlmachine_model:name(Model).

-spec model_name(Assembly::assembly(), Name::atom()) -> assembly().
model_name(Assembly, Name) ->
    Model = model(Assembly),
    model(Assembly, erlmachine_model:name(Model, Name)).

-spec model_options(Assembly::assembly()) -> list().
model_options(Assembly) ->
    Model = model(Assembly),
    erlmachine_model:options(Model).

-spec model_options(Assembly::assembly(), Opt::list()) -> assembly().
model_options(Assembly, Opt) ->
    Model = model(Assembly),
    model(Assembly, erlmachine_model:options(Model, Opt)).

-spec model_digest(Assembly::assembly()) -> binary().
model_digest(Assembly) ->
    Model = model(Assembly),
    erlmachine_model:digest(Model).

-spec model_digest(Assembly::assembly(), Digest::binary()) -> assembly().
model_digest(Assembly, Digest) ->
    Model = model(Assembly),
    model(Assembly, erlmachine_model:digest(Model, Digest)).


-spec product(Assembly::assembly()) -> product().
product(Assembly) ->
    Model = model(Assembly),
    erlmachine_model:product(Model).

-spec product(Assembly::assembly(), Product::product()) -> assembly().
product(Assembly, Product) ->
    Model = model(Assembly),
    model(Assembly, erlmachine_model:product(Model, Product)).

-spec prototype() -> prototype().
prototype() ->
    erlmachine_prototype:prototype().

-spec prototype_name(Assembly::assembly()) -> Name::atom().
prototype_name(Assembly) ->
    Model = model(Assembly),
    Prototype = erlmachine_model:prototype(Model),
    erlmachine_prototype:name(Prototype).

-spec prototype_name(Assembly::assembly(), Name::atom()) -> Release::assembly().
prototype_name(Assembly, Name) ->
    Model = model(Assembly),
    Prototype = erlmachine_model:prototype(Model),
    prototype(Assembly, erlmachine_prototype:name(Prototype, Name)).

-spec prototype_body(Assembly::assembly()) -> term().
prototype_body(Assembly) ->
    Model = model(Assembly),
    Prototype = erlmachine_model:prototype(Model),
    erlmachine_prototype:body(Prototype).

-spec prototype_body(Assembly::assembly(), Body::term()) -> Release::assembly().
prototype_body(Assembly, Body) ->
    Model = model(Assembly),
    Prototype = erlmachine_model:prototype(Model),
    prototype(Assembly, erlmachine_prototype:body(Prototype, Body)).

-spec prototype_options(Assembly::assembly()) -> list().
prototype_options(Assembly) ->
    Model = model(Assembly),
    Prototype = erlmachine_model:prototype(Model),
    erlmachine_prototype:options(Prototype).

-spec prototype_options(Assembly::assembly(), Opt::list()) -> assembly().
prototype_options(Assembly, Opt) ->
    Model = model(Assembly),
    Prototype = erlmachine_model:prototype(Model),
    prototype(Assembly, erlmachine_prototype:options(Prototype, Opt)).

-spec parts(Assembly::assembly()) -> list(assembly()).
parts(Assembly) ->
    Assembly#assembly.parts.

-spec parts(Assembly::assembly(), Parts::list(assembly())) -> assembly().
parts(Assembly, Parts) ->
    Assembly#assembly{ parts=Parts }.

-spec mounted(Assembly::assembly()) -> assembly().
mounted(Assembly) ->
    Assembly#assembly.mounted.

-spec mounted(Assembly::assembly(), Mount::assembly()) -> assembly().
mounted(Assembly, Mount) ->
    Assembly#assembly{ mounted=parts(Mount, []) }.

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
    Assembly#assembly{label = Label}.

-spec add(Assembly::assembly(), Part::assembly()) -> assembly().
add(Assembly, Part) ->
    Parts = lists:keystore(serial_no(Part), serial_no(), parts(Assembly), Part),
    parts(Assembly, Parts).

-spec remove(Assembly::assembly(), Label::term()) -> assembly().
remove(Assembly, Label) ->
    Parts = lists:keydelete(Label, #assembly.label, parts(Assembly)),
    parts(Assembly, Parts).

-spec part(Assembly::assembly(), Label::term()) -> assembly().
part(Assembly, Label) ->
    lists:keyfind(Label, #assembly.label, parts(Assembly)).

-spec tabname() -> atom().
tabname() -> 
    ?MODULE.

-spec record_name() -> atom().
record_name() ->
    assembly.

-spec attributes() -> list(atom()).
attributes() ->
    record_info(fields, assembly).

-spec create(Assembly::assembly()) ->
                    success(serial_no()).
create(Assembly) ->
    ID = serial_no(Assembly),
    ok = mnesia:dirty_write(tabname(), Assembly),
    {ok, ID}.

-spec read(ID::serial_no()) -> 
                  success(assembly()).
read(ID) ->
    [Assembly] = mnesia:dirty_read(tabname(), ID),
    {ok, Assembly}.

-spec update(Assembly::assembly()) -> 
                    success().
update(Assembly) ->
    ok = mnesia:dirty_write(tabname(), Assembly).

-spec delete(ID::serial_no()) -> 
                    success().
delete(ID) ->
    ok = mnesia:dirty_delete(tabname(), ID).

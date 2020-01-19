-module(erlmachine_assembly).

%% API.

-export([assembly/0, assembly/2, model/0, prototype/0]).

-export([
         install/1, install/2,
         attach/3, attach/4, attach_to_label/4, attach_by_label/4, attach_by_serial_no/4,
         detach/2, detach/3, detach_from_label/3, detach_by_label/3, detach_by_serial_no/3,
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

-export([add_part/2, remove_part/2, get_part/2]).

-export([labels/1]).

-export([tabname/0, record_name/0, attributes/0]).

-export([by_serial_no/1]).

-export([load/1, unload/1]).

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

-type label() :: atom().

-export_type([assembly/0]).

-spec tabname() -> atom().
tabname() -> 
    assembly.

record_name() ->
    assembly.

-spec attributes() -> list(atom()).
attributes() ->
    record_info(fields, assembly).

%% TODO We need to provide methods like by_model_no, by_sum.
-spec by_serial_no(SN::serial_no()) -> 
                          success(assembly()) | failure(term(), term()).
by_serial_no(SN) ->
    Result = mnesia:dirty_read(tabname(), SN),
    {ok, Result}.

-spec load(Assembly::assembly()) -> 
                  success().
load(Assembly) when is_record(Assembly, assembly) ->
    ok = mnesia:dirty_write(Assembly).

-spec unload(SN::serial_no()) -> 
                    success().
unload(SN) ->
    ok = mnesia:dirty_delete(tabname(), SN).

-spec install(GearBox::assembly()) ->
                     success(pid()) | ingnore | failure(E::term()).
install(GearBox) ->
    SN = serial_no(GearBox),
    Options = prototype_options(GearBox),
    Schema = erlmachine_gearbox:schema(GearBox),
    %% TODO at that place we can register information about scheme in persistence storage;
    Result = (prototype_name(GearBox)):install(SN, GearBox, Options),
    erlmachine_schema:add_edges(Schema, GearBox),
    Result.

-spec install(GearBox::assembly(), Assembly::assembly()) ->
                     success(pid()) | ingnore | failure(E::term()).
install(GearBox, Assembly) ->
    SN = serial_no(Assembly),
    Options = prototype_options(Assembly),
    (prototype_name(Assembly)):install(SN, GearBox, Assembly, Options).

-spec attach(GearBox::assembly(), Register::term(), Part::assembly()) -> 
                    success(term()) | failure(term(), term()).
attach(GearBox, Register, Part) ->
    SN = serial_no(GearBox),
    Result = (prototype_name(GearBox)):attach(SN, GearBox, Register, Part), 
    erlmachine_schema:add_edge(erlmachine_gearbox:schema(GearBox), SN, Part),
    Result.

-spec attach(GearBox::assembly(), SN::serial_no(), Register::term(), Part::assembly()) -> 
                    success(term()) | failure(term(), term()).
attach(GearBox, SN, Register, Part) ->
    Assembly = erlmachine_gearbox:find(GearBox, SN),
    Result = (prototype_name(Assembly)):attach(SN, GearBox, Assembly, Register, Part), 
    erlmachine_schema:add_edge(erlmachine_gearbox:schema(GearBox), SN, Part),
    Result.

-spec attach_to_label(GearBox::assembly(), Label::term(), Register::term(), Part::assembly()) -> 
                             success(term()) | failure(term(), term()).
attach_to_label(GearBox, Label, Register, Part) ->
    Labels = labels(GearBox),
    SN = maps:get(Label, Labels), %% TODO Label's access can be incapsulated in separated function;
    attach(GearBox, SN, Register, Part).

-spec attach_by_label(GearBox::assembly(), SN::serial_no(), Register::term(), Label::term()) -> 
                             success(term()) | failure(term(), term()).
attach_by_label(GearBox, SN, Register, Label) ->
    Labels = labels(GearBox),
    Part =  erlmachine_gearbox:find(GearBox, maps:get(Label, Labels)),
    attach(GearBox, SN, Register, Part).

-spec attach_by_serial_no(GearBox::assembly(), SN::serial_no(), Register::term(), ID::serial_no()) -> 
                             success(term()) | failure(term(), term()).
attach_by_serial_no(GearBox, SN, Register, ID) ->
    Part =  erlmachine_gearbox:find(GearBox, ID),
    attach(GearBox, SN, Register, Part).

-spec detach(GearBox::assembly(), ID::serial_no()) -> 
                    success(term()) | failure(term(), term()).
detach(GearBox, ID) ->
    SN = serial_no(GearBox),
    Result = (prototype_name(GearBox)):detach(SN, GearBox, ID),
    ok = erlmachine_schema:del_vertex(erlmachine_gearbox:schema(GearBox), ID),
    Result.

-spec detach(GearBox::assembly(), SN::serial_no(), ID::serial_no()) -> 
                    success(term()) | failure(term(), term()).
detach(GearBox, SN, ID) ->
    Assembly = erlmachine_gearbox:find(GearBox, SN),
    Result = (prototype_name(Assembly)):detach(SN, GearBox, Assembly, ID),
    ok = erlmachine_schema:del_vertex(erlmachine_gearbox:schema(GearBox), ID),
    Result.

-spec detach_from_label(GearBox::assembly(), Label::term(), ID::serial_no()) -> 
                    success(term()) | failure(term(), term()).
detach_from_label(GearBox, Label, ID) ->
    Labels = labels(GearBox),
    SN = maps:get(Label, Labels),
    detach(GearBox, SN, ID). 

-spec detach_by_label(GearBox::assembly(), SN::serial_no(), Label::term()) -> 
                               success(term()) | failure(term(), term()).
detach_by_label(GearBox, SN, Label) ->
    Labels = labels(GearBox),
    ID = maps:get(Label, Labels),
    detach(GearBox, SN, ID). 

-spec detach_by_serial_no(GearBox::assembly(), SN::serial_no(), ID::serial_no()) -> 
                             success(term()) | failure(term(), term()).
detach_by_serial_no(GearBox, SN, ID) ->
    detach(GearBox, SN, ID). 

-spec uninstall(GearBox::assembly(), Reason::term()) ->
                     ok.
uninstall(GearBox, Reason) ->
    SN = serial_no(GearBox),
    Result = (prototype_name(GearBox)):uninstall(SN, GearBox, Reason),
    ok = erlmachine_schema:del_vertex(erlmachine_gearbox:schema(GearBox), SN),
    Result.

-spec uninstall(GearBox::assembly(), SN::serial_no(), Reason::term()) ->
                       ok.
uninstall(GearBox, SN, Reason) ->
    Assembly = erlmachine_gearbox:find(GearBox, SN),
    (prototype_name(GearBox)):uninstall(SN, GearBox, Assembly, Reason).

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
    Result = (prototype_name(GearBox)):attached(SN, GearBox, Part, Extension),
    Result.

-spec detached(GearBox::assembly(), Part::assembly(), ID::serial_no()) -> 
                      ok.
detached(GearBox, Part, ID) ->
    SN = serial_no(GearBox),
    Result = (prototype_name(GearBox)):detached(SN, GearBox, Part, ID),
    Result.

-spec replaced(GearBox::assembly(), Assembly::assembly(), Part::assembly()) -> 
                      ok.
replaced(GearBox, Assembly, Part) ->
    SN = serial_no(GearBox),
    Result = (prototype_name(GearBox)):replaced(SN, GearBox, Assembly, Part),
    Result.

-spec overloaded(GearBox::assembly(), Assembly::assembly(), Load::term()) -> 
                      ok.
overloaded(GearBox, Assembly, Load) ->
    SN = serial_no(GearBox),
    Result = (prototype_name(GearBox)):overloaded(SN, GearBox, Assembly, Load),
    Result.

-spec blocked(GearBox::assembly(), Assembly::assembly(), Part::assembly(), Failure::term()) -> 
                        ok.
blocked(GearBox, Assembly, Part, Failure) ->
    SN = serial_no(GearBox),
    Result =  (prototype_name(GearBox)):blocked(SN, GearBox, Assembly, Part, Failure),
    Result.

-spec uninstalled(GearBox::assembly(), Assembly::assembly(), Reason::term()) -> 
                       ok.
uninstalled(GearBox, Assembly, Reason) ->
    Mounted = mounted(Assembly),
    SN0 = serial_no(Mounted), SN1 = serial_no(GearBox),
    (prototype_name(GearBox)):uninstalled(SN1, GearBox, Assembly, Reason),
    (SN1 == SN0) orelse 
                   (prototype_name(Mounted)):uninstalled(SN0, GearBox, Mounted, Assembly, Reason),
    %% NOTE Instead of access from external process we are going to provide
    %% notification and update monitoring copy with suitable tags, etc.;
    ok.

-spec assembly() -> assembly().
assembly() ->
    #assembly{}.

-spec assembly(SN::serial_no(), Model::model()) -> assembly().
assembly(SN, Model) ->
    #assembly{ serial_no=SN, model=Model }.

-spec is_mounted(Assembly::assembly()) -> boolean().
is_mounted(Assembly) -> 
    mounted(Assembly) /= undefined.

-spec serial_no() -> integer().
serial_no() ->
    #assembly.serial_no.

-spec serial_no(Assembly::assembly()) -> SN::serial_no().
serial_no(Assembly) ->
    SN = Assembly#assembly.serial_no,
    SN.

-spec serial_no(Assembly::assembly(), SN::serial_no()) -> Release::assembly().
serial_no(Assembly, SN) ->
    Release = Assembly#assembly{ serial_no=SN },
    Release.

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

-spec add_part(Assembly::assembly(), Part::assembly()) -> assembly().
add_part(Assembly, Part) ->
    Parts = lists:keystore(serial_no(Part), serial_no(), parts(Assembly), Part),
    parts(Assembly, Parts).

-spec remove_part(Assembly::assembly(), ID::serial_no()) -> assembly().
remove_part(Assembly, ID) ->
    Parts = lists:keydelete(ID, #assembly.serial_no, parts(Assembly)),
    parts(Assembly, Parts).

-spec get_part(Assembly::assembly(), ID::serial_no()) -> assembly().
get_part(Assembly, ID) ->
    lists:keyfind(ID, #assembly.serial_no, parts(Assembly)).

-spec labeled(Assembly::assembly()) -> list().
labeled(Assembly) ->
    Label = label(Assembly),
    if Label == undefined ->
            #{};
       true  ->
            #{Label => serial_no(Assembly)}
    end.

-spec labels(Assembly::assembly()) -> map().
labels(Assembly) ->
    labels(Assembly, #{}).

-spec labels(Assembly::assembly(), Acc::map()) -> map().
labels(Assembly, Acc) ->
    Parts = parts(Assembly),
    Labeled = labeled(Assembly),
    if Parts == [] ->
            maps:merge(Labeled, Acc);
       true ->
            maps:merge(Labeled, lists:foldl(fun labels/2, Acc, Parts))
    end.

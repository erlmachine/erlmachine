-module(erlmachine_assembly).
-behaviour(gen_server).

%% API.
-export([start_link/0]).

%% gen_server.
-export([
         init/1,
         handle_call/3, handle_cast/2, handle_info/2,
         terminate/2,
         code_change/3
        ]).

-export([assembly/0, model/0, prototype/0]).

-export([
         install/1, install/2,
         attach/3, detach/3,
         mount/2, mount/3, 
         unmount/2, unmount/3, 
         uninstall/2, uninstall/3
        ]).

-export([
         installed/3, 
         attached/3, detached/3,
         replaced/3,
         accepted/4, rejected/4,
         overloaded/3, blocked/4,
         uninstalled/4
        ]).

-export([
         is_mounted/1,
         serial_no/1, serial_no/2,
         model/1, model/2, 
         model_name/1, model_name/2,
         model_options/1, model_options/2,
         prototype/1, prototype/2, 
         prototype_name/1, prototype_name/2,
         prototype_options/1, prototype_options/2,
         assembly_options/1, assembly_options/2,
         model_no/1, model_no/2,
         part_no/1, part_no/2,
         product/1, product/2,
         parts/1, parts/2,
         mounted/1, mounted/2,
         tags/1, tags/2,
         label/1, label/2
        ]).

-export([add_part/2, remove_part/2, get_part/2]).

-export([spec/2]).

-export([labels/1]).

-include("erlmachine_system.hrl").


%% The main purpose of this module is to instantiate proceses accordingly to design file;
%% In this module will be provided incapsulation around building of independent parts and whole transmission too;

-type serial_no() :: erlmachine_serial_number:serial_no().

-type model_no() :: term().
-type part_no() :: term().

-type gear() :: erlmachine_gear:gear().
-type axle() :: erlmachine_axle:axle().
-type shaft() :: erlmachine_shaft:shaft().
-type gearbox() :: erlmachine_gearbox:gerbox().

-type product() :: gear() | axle() | gearbox() | shaft().

%% Abbreviations M/N and P/N will be represented on name;

-record(model, {
                name::atom(),
                model_no::model_no(),
                product::product(),
                options::term()
               }
       ).

-record(prototype, {
                    name::atom(),
                    options::term()
                   }
       ).

-type model() :: #model{}.
-type prototype() :: #prototype{}.

-record (assembly, {
                    serial_no::serial_no(), %% We can get build info (ts, etc..) by serial number from db;
                    prototype::prototype(),

                    model::model(),
                    mounted::assembly(),
                    parts=[]::list(assembly()),
                    part_no::part_no(),
                    options=[]::list(),
                    tags=[]::list(term()),
                    label::label()
                   }
        ).

-type assembly() :: #assembly{}.

-type label() :: atom().

-type acceptance_criteria() :: list().
-type accept() :: true.
-type reject() :: list().

-export_type([assembly/0, model/0, prototype/0, product/0]).
-export_type([model_no/0, part_no/0]).
-export_type([acceptance_criteria/0, accept/0, reject/0]).

-spec install(GearBox::assembly()) ->
                     success(pid()) | ingnore | failure(E::term()).
install(GearBox) ->
    SN = serial_no(GearBox),
    Options = prototype_options(GearBox),
    %% TODO at that place we can register information about scheme in persistence storage;
    Result = (prototype_name(GearBox)):install(SN, GearBox, Options),
    ok = erlmachine_gearbox:map(GearBox),
    Result.

-spec install(GearBox::assembly(), Assembly::assembly()) ->
                     success(pid()) | ingnore | failure(E::term()).
install(GearBox, Assembly) ->
    SN = serial_no(Assembly),
    Options = prototype_options(Assembly),
    Result = (prototype_name(Assembly)):install(SN, GearBox, Assembly, Options),
    Result.

-spec attach(GearBox::assembly(), SN::serial_no(), Part::assembly()) -> 
                    success(term()) | failure(term(), term()).
attach(GearBox, SN, Part) ->
    Assembly = erlmachine_gearbox:find(GearBox, SN),
    Result = (prototype_name(Assembly)):attach(SN, GearBox, Assembly, Part),
    ok = erlmachine_gearbox:map_add(GearBox, Assembly, Part),
    Result.

-spec detach(GearBox::assembly(), SN::serial_no(), ID::serial_no()) -> 
                    success(term()) | failure(term(), term()).
detach(GearBox, SN, ID) ->
    Assembly = erlmachine_gearbox:find(GearBox, SN),
    Result = (prototype_name(Assembly)):detach(SN, GearBox, Assembly, ID),
    ok = erlmachine_gearbox:map_remove(GearBox, ID),
    Result.

-spec mount(GearBox::assembly(), Part::assembly()) -> 
                    success(term()) | failure(term(), term()).
mount(GearBox, Part) ->
    SN = serial_no(GearBox),
    %% At that palce we need to update stored schema;
    Result = (prototype_name(GearBox)):mount(SN, GearBox, Part),
    ok = erlmachine_gearbox:map_add(GearBox, Part),
    Result.

-spec mount(GearBox::assembly(), SN::serial_no(), Part::assembly()) -> 
                   success(term()) | failure(term(), term()).
mount(GearBox, SN, Part) ->
    Assembly = erlmachine_gearbox:find(GearBox, SN),
    Result = (prototype_name(Assembly)):mount(SN, GearBox, Assembly, Part),
    ok = erlmachine_gearbox:map_add(GearBox, Assembly, Part),
    Result.

%% The main difference between unmount and uninstall:
%% is that second one can be able to stop chield and mark it with stopped label without removing of edge;

-spec unmount(GearBox::assembly(), ID::serial_no()) -> 
                    success(term()) | failure(term(), term()).
unmount(GearBox, ID) ->
    SN = serial_no(GearBox),
    Result = (prototype_name(GearBox)):unmount(SN, GearBox, ID),
    ok = erlmachine_gearbox:map_remove(GearBox, ID),
    Result.

-spec unmount(GearBox::assembly(), SN::serial_no(), ID::serial_no()) -> 
                     success(term()) | failure(term(), term()).
unmount(GearBox, SN, ID) ->
    Assembly = erlmachine_gearbox:find(GearBox, SN),
    Result = (prototype_name(Assembly)):unmount(SN, GearBox, Assembly, ID),
    ok = erlmachine_gearbox:map_remove(GearBox, ID),
    Result.

-spec uninstall(GearBox::assembly(), Reason::term()) ->
                     ok.
uninstall(GearBox, Reason) ->
    SN = serial_no(GearBox),
    Result = (prototype_name(GearBox)):uninstall(SN, GearBox, Reason),
    ok = erlmachine_gearbox:map_remove(GearBox, SN),
    Result.

-spec uninstall(GearBox::assembly(), SN::serial_no(), Reason::term()) ->
                       ok.
uninstall(GearBox, SN, Reason) ->
    Assembly = erlmachine_gearbox:find(GearBox, SN),
    Result = (prototype_name(GearBox)):uninstall(SN, GearBox, Assembly, Reason),
    Result.

-spec installed(GearBox::assembly(), Mounted::assembly(), Part::assembly()) -> 
                         ok.
installed(GearBox, Mounted, Part) ->
    MountedSN = serial_no(Mounted), GearBoxSN = serial_no(GearBox),
    (prototype_name(GearBox)):installed(GearBoxSN, GearBox, Part),
    (GearBoxSN == MountedSN) orelse 
                               (prototype_name(Mounted)):installed(MountedSN, GearBox, Mounted, Part),
    %% NOTE Instead of access from external process we are going to provide
    %% notification and update monitoring copy with suitable tags, etc.;
    ok.

-spec attached(GearBox::assembly(), Assembly::assembly(), Part::assembly()) -> 
                       ok.
attached(_GearBox, Assembly, Part) ->
    SN = serial_no(Assembly),
    Result = (prototype_name(Assembly)):attached(SN, Assembly, Part),
    Result.

-spec detached(GearBox::assembly(), Assembly::assembly(), ID::serial_no()) -> 
                      ok.
detached(_GearBox, Assembly, ID) ->
    SN = serial_no(Assembly),
    Result = (prototype_name(Assembly)):detached(SN, Assembly, ID),
    Result.

-spec replaced(GearBox::assembly(), Assembly::assembly(), Part::assembly()) -> 
                      ok.
replaced(GearBox, Assembly, Part) ->
    SN = serial_no(Assembly),
    Result = (prototype_name(GearBox)):replaced(SN, GearBox, Assembly, Part),
    Result.

-spec accepted(GearBox::assembly(), Assembly::assembly(), Criteria::term(), Report::term()) -> 
                      ok.
accepted(GearBox, Assembly, Criteria, Report) ->
    SN = serial_no(Assembly),
    Result = (prototype_name(GearBox)):accepted(SN, GearBox, Assembly, Criteria, Report),
    Result.

-spec rejected(GearBox::assembly(), Assembly::assembly(), Criteria::term(), Report::term()) -> 
                      ok.
rejected(GearBox, Assembly, Criteria, Report) ->
    SN = serial_no(Assembly),
    Result = (prototype_name(GearBox)):rejected(SN, GearBox, Assembly, Criteria, Report),
    Result.

-spec overloaded(GearBox::assembly(), Assembly::assembly(), Load::term()) -> 
                      ok.
overloaded(GearBox, Assembly, Load) ->
    SN = serial_no(Assembly),
    Result = (prototype_name(GearBox)):overloaded(SN, GearBox, Assembly, Load),
    Result.

-spec blocked(GearBox::assembly(), Assembly::assembly(), Part::assembly(), Failure::term()) -> 
                        ok.
blocked(GearBox, Assembly, Part, Failure) ->
    SN = serial_no(Assembly),
    Result =  (prototype_name(GearBox)):blocked(SN, GearBox, Assembly, Part, Failure),
    Result.

-spec uninstalled(GearBox::assembly(), Mounted::assembly(), Part::assembly(), Reason::term()) -> 
                       ok.
uninstalled(GearBox, Mounted, Part, Reason) ->
    MountedSN = serial_no(Mounted), GearBoxSN = serial_no(GearBox),
    (prototype_name(GearBox)):uninstalled(GearBoxSN, GearBox, Part, Reason),
    (GearBoxSN == MountedSN) orelse 
                               (prototype_name(Mounted)):uninstalled(MountedSN, GearBox, Mounted, Part, Reason),
    %% NOTE Instead of access from external process we are going to provide
    %% notification and update monitoring copy with suitable tags, etc.;
    ok.

%% API.

-spec start_link() -> {ok, pid()}.
start_link() ->
	gen_server:start_link(?MODULE, [], []).

%% gen_server.

-record(state, {}).

init([]) ->
	{ok, #state{}}.

handle_call(_Request, _From, State) ->
	{reply, ignored, State}.

handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

-spec assembly() -> assembly().
assembly() ->
    #assembly{}.

-spec is_mounted(Assembly::assembly()) -> boolean().
is_mounted(Assembly) -> 
    mounted(Assembly) /= undefined.

-spec serial_no(Assembly::assembly()) -> SN::serial_no().
serial_no(Assembly) ->
    SN = Assembly#assembly.serial_no,
    SN.

-spec serial_no(Assembly::assembly(), SN::serial_no()) -> Release::assembly().
serial_no(Assembly, SN) ->
    Release = Assembly#assembly{serial_no=SN},
    Release.

-spec model() -> model().
model() ->
    #model{}.

-spec model(Assembly::assembly()) -> Model::model().
model(Assembly) ->
    Model = Assembly#assembly.model,
    Model.

-spec model(Assembly::assembly(), Model::model()) -> Release::assembly().
model(Assembly, Model) ->
    Release = Assembly#assembly{model = Model},
    Release.

-spec prototype(Assembly::assembly()) -> Prototype::prototype().
prototype(Assembly) ->
    Prototype = Assembly#assembly.prototype,
    Prototype.

-spec prototype(Assembly::assembly(), Prototype::prototype()) -> Release::assembly().
prototype(Assembly, Prototype) ->
    Release = Assembly#assembly{prototype = Prototype},
    Release.

-spec model_no(Assembly::assembly()) -> MN::term().
model_no(Assembly) ->
    Model = model(Assembly),
    MN = Model#model.model_no,
    MN.

-spec model_no(Assembly::assembly(), MN::term()) -> Release::assembly().
model_no(Assembly, MN) ->
    Model = model(Assembly),
    Release = model(Assembly, Model#model{model_no=MN}),
    Release.

-spec model_name(Assembly::assembly()) -> Name::atom().
model_name(Assembly) ->
    Model = model(Assembly),
    Name = Model#model.name, 
    Name.

-spec model_name(Assembly::assembly(), Name::atom()) -> Release::assembly().
model_name(Assembly, Name) ->
    Model = model(Assembly),
    Release = model(Assembly, Model#model{name=Name}),
    Release.

-spec prototype() -> prototype().
prototype() ->
    #prototype{}.

-spec prototype_name(Assembly::assembly()) -> Name::atom().
prototype_name(Assembly) ->
    Prototype = prototype(Assembly),
    Name =  Prototype#prototype.name,
    Name.

-spec prototype_name(Assembly::assembly(), Name::atom()) -> Release::assembly().
prototype_name(Assembly, Name) ->
    Prototype = prototype(Assembly),
    Release = prototype(Assembly, Prototype#prototype{name=Name}),
    Release.

-spec model_options(Assembly::assembly()) -> Options::list().
model_options(Assembly) ->
    Model = model(Assembly),
    Options = Model#model.options,
    Options.

-spec model_options(Assembly::assembly(), Options::list()) -> Release::assembly().
model_options(Assembly, Options) ->
    Model = model(Assembly),
    Release = model(Assembly, Model#model{options=Options}),
    Release.

-spec prototype_options(Assembly::assembly()) -> Options::list().
prototype_options(Assembly) ->
    Prototype = prototype(Assembly),
    Options = Prototype#prototype.options,
    Options.

-spec prototype_options(Assembly::assembly(), Options::list()) -> Release::assembly().
prototype_options(Assembly, Options) ->
    Prototype = prototype(Assembly),
    Release = prototype(Assembly, Prototype#prototype{options=Options}),
    Release.

-spec assembly_options(Assembly::assembly()) -> Options::list().
assembly_options(Assembly) ->
    Options = Assembly#assembly.options,
    Options.

-spec assembly_options(Assembly::assembly(), Options::list()) -> Release::assembly().
assembly_options(Assembly, Options) ->
    Release = Assembly#assembly{options=Options},
    Release.

-spec product(Assembly::assembly()) -> Product::product().
product(Assembly) ->
    Model = model(Assembly),
    Product = Model#model.product,
    Product.

-spec product(Assembly::assembly(), Product::product()) -> Release::assembly().
product(Assembly, Product) ->
    Model = model(Assembly),
    Release = model(Assembly, Model#model{product=Product}),
    Release.

-spec parts(Assembly::assembly()) -> list(assembly()).
parts(Assembly) ->
    Assembly#assembly.parts.

-spec parts(Assembly::assembly(), Parts::list(assembly())) -> assembly().
parts(Assembly, Parts) ->
    Assembly#assembly{parts=Parts}.

-spec mounted(Assembly::assembly()) -> assembly().
mounted(Assembly) ->
    Assembly#assembly.mounted.

-spec mounted(Assembly::assembly(), Mount::assembly()) -> assembly().
mounted(Assembly, Mount) ->
    Assembly#assembly{mounted=parts(Mount, [])}.

-spec part_no(Assembly::assembly()) -> PN::term().
part_no(Assembly) ->
    PN = Assembly#assembly.part_no,
    PN.

-spec part_no(Assembly::assembly(), PN::term()) -> assembly().
part_no(Assembly, PN) ->
    Release = Assembly#assembly{part_no=PN},
    Release.

-spec add_part(Assembly::assembly(), Part::assembly()) -> assembly().
add_part(Assembly, Part) ->
    Parts = lists:reverse([Part|parts(Assembly)]),
    Release = parts(Assembly, Parts),
    Release.

-spec remove_part(Assembly::assembly(), ID::serial_no()) -> assembly().
remove_part(Assembly, ID) ->
    Parts = lists:keydelete(ID, #assembly.serial_no, parts(Assembly)),
    Release = parts(Assembly, Parts),
    Release.

-spec get_part(Assembly::assembly(), ID::serial_no()) -> assembly().
get_part(Assembly, ID) ->
    Part = lists:keyfind(ID, #assembly.serial_no, parts(Assembly)),
    Part.

-spec tags(Assembly::assembly()) -> Tags::term().
tags(Assembly) ->
    Tags = Assembly#assembly.tags,
    Tags.

-spec tags(Assembly::assembly(), Tags::term()) -> assembly().
tags(Assembly, Tags) ->
    Release = Assembly#assembly{tags=Tags},
    Release.

-spec label(Assembly::assembly()) -> label().
label(Assembly) ->
    Label = Assembly#assembly.label,
    Label.

-spec label(Assembly::assembly(), Label::label()) -> assembly().
label(Assembly, Label) ->
    Release = Assembly#assembly{label = Label},
    Release.

-spec labeled(Assembly::assembly()) -> list().
labeled(Assembly) ->
    Label = label(Assembly),
    if Label == indefined ->
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

%% TODO
%% I am going to provide mnesia gears, mongodb , etc..
%% Process manager will be responsible for persistance storage management

-spec spec(GearBox::assembly(), Part::assembly()) -> Spec::map().
spec(GearBox, Part) ->
    SN = erlmachine_assembly:serial_no(Part),
    Module = erlmachine_assembly:prototype_name(Part),
    Opt = erlmachine_assembly:prototype_options(Part),
    Start = {Module, install, [SN, parts(GearBox,[]), Part, Opt]},
    AssemblyOpt = erlmachine_assembly:assembly_options(Part),
    Restart = proplists:get_value(restart, AssemblyOpt, permanent),
    Shutdown = proplists:get_value(shutdown, AssemblyOpt, 5000),
    Modules = proplists:get_value(modules, AssemblyOpt, [Module]),
    Type = proplists:get_value(type, AssemblyOpt),
    #{id => SN, start => Start, restart => Restart, shutdown => Shutdown, modules => Modules, type => Type}.

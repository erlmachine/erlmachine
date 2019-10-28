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

-export([install/1, install/2, uninstall/3]).

-export([
         serial_no/1, serial_no/2,
         model/1, model/2, model_name/1, model_name/2, model_options/1, model_options/2,
         prototype/1, prototype/2, prototype_name/1, prototype_name/2, prototype_options/1, prototype_options/2,
         assembly_options/1, assembly_options/2,
         model_no/1, model_no/2,
         part_no/1, part_no/2,
         product/1, product/2,
         parts/1, parts/2,
         mount/1, mount/2
        ]).

-export([attach/2, detach/2, part/2]).

-export([spec/2]).

-include("erlmachine_system.hrl").


%% The main purpose of this module is to instantiate proceses accordingly to design file;
%% In this module will be provided incapsulation around building of independent parts and whole transmission as well;

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
                    mount::assembly(),
                    parts::list(assembly()),
                    part_no::part_no(),
                    options::list()
                   }
        ).

-type assembly() :: #assembly{}.

-type acceptance_criteria() :: list().
-type accept() :: true.
-type reject() :: list().

-export_type([assembly/0, model/0, prototype/0, product/0]).
-export_type([model_no/0, part_no/0]).
-export_type([acceptance_criteria/0, accept/0, reject/0]).

-spec install(Assembly::assembly()) ->
                     success(pid()) | ingnore | failure(E::term()).
install(Assembly) ->
    SN = serial_no(Assembly),
    Options = prototype_options(Assembly),
    (erlmachine_assembly:prototype_name(Assembly)):install(SN, Assembly, Options).

-spec install(GearBox::assembly(), Assembly::assembly()) ->
                     success(pid()) | ingnore | failure(E::term()).
install(GearBox, Assembly) ->
    SN = serial_no(Assembly),
    Options = prototype_options(Assembly),
    (erlmachine_assembly:prototype_name(Assembly)):install(SN, GearBox, Assembly, Options).

-spec uninstall(Assembly::assembly(), Reason::term(), TimeOut::integer()) ->
                     ok.
uninstall(Assembly, Reason, TimeOut) ->
    SN = serial_no(Assembly),
    (erlmachine_assembly:prototype_name(Assembly)):uninstall(SN, Reason, TimeOut).

%% Client doesn't need to know about mount method;
%% I guess it's responsibility of transmission (attach call); 

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

-spec mount(Assembly::assembly()) -> assembly().
mount(Assembly) ->
    Assembly#assembly.mount.

-spec mount(Assembly::assembly(), Mount::assembly()) -> assembly().
mount(Assembly, Mount) ->
    Assembly#assembly{mount=Mount}.

-spec part_no(Assembly::assembly()) -> PN::term().
part_no(Assembly) ->
    PN = Assembly#assembly.part_no,
    PN.

-spec part_no(Assembly::assembly(), PN::term()) -> assembly().
part_no(Assembly, PN) ->
    Release = Assembly#assembly{part_no=PN},
    Release.

-spec part(Assembly::assembly(), ID::serial_no()) -> assembly().
part(Assembly, ID) ->
    Part = lists:keyfind(ID, #assembly.serial_no, parts(Assembly)),
    Part.

-spec attach(Assembly::assembly(), Part::assembly()) -> assembly().
attach(Assembly, Part) ->
    Parts = lists:reverse([Part|parts(Assembly)]),
    Release = erlmachine_assembly:parts(Assembly, Parts),
    Release.

-spec detach(Assembly::assembly(), ID::serial_no()) -> assembly().
detach(Assembly, ID) ->
    Parts = lists:keydelete(ID, #assembly.serial_no, parts(Assembly)),
    Release = erlmachine_assembly:parts(Assembly, Parts),
    Release.

-spec spec(GearBox::assembly(), Part::assembly()) -> map().
spec(GearBox, Part) ->
    SN = erlmachine_assembly:serial_no(Part),
    Module = erlmachine_assembly:prototype_name(Part),
    Opt = erlmachine_assembly:prototype_options(Part),
    Start = {Module, install, [SN, GearBox, Part, Opt]},
    AssemblyOpt = erlmachine_assembly:assembly_options(Part),
    Restart = proplists:get_value(restart, AssemblyOpt, permanent),
    Shutdown = proplists:get_value(shutdown, AssemblyOpt, 5000),
    Modules = proplists:get_value(modules, AssemblyOpt, [Module]),
    Type = proplists:get_value(type, AssemblyOpt),
    #{id => SN, start => Start, restart => Restart, shutdown => Shutdown, modules => Modules, type => Type}.

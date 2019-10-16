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

-export([gear/5]).

-export([attach/2, detach/2, switch/2]).

-export([
         serial_no/1, serial_no/2,
         model/1, model/2, model_name/1, model_name/2, model_options/1, model_options/2,
         prototype/1, prototype/2, prototype_name/1, prototype_name/2,
         model_no/1, model_no/2,
         part_no/1, part_no/2,
         product/1, product/2,
         parts/1, parts/2,
         mount/1, mount/2
        ]).

-export([install_model/1, replace_model/3, uninstall_model/3, accept_model/3]).

-export([installed/2, replaced/3, uninstalled/3, accepted/4, rejected/4]).

-include("erlmachine_system.hrl").

-callback install(SN::serial_no(), MN::model_no(), PN::part_no(), Options::list()) -> 
    success(term()) | failure(term(), term(), term()) | failure(term()).

-callback replace(SN::serial_no(), ID::serial_no(), Body::term()) -> 
    success(term()) | failure(term(), term(), term()) | failure(term()).

-callback uninstall(SN::serial_no(), Reason::term(), Body::term()) -> 
    success(term()) | failure(term(), term(), term()) | failure(term()).

-callback accept(SN::serial_no(), Criteria::term(), Body::term()) -> 
    success(term(), term()) | failure(term(), term(), term()) | failure(term()).

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
                    part_no::part_no()
                   }
        ).

-type assembly() :: #assembly{}.

-type acceptance_criteria() :: list().
-type accept() :: true.
-type reject() :: list().

-export_type([assembly/0, model/0, prototype/0, product/0]).
-export_type([model_no/0, part_no/0]).
-export_type([acceptance_criteria/0, accept/0, reject/0]).

%% API.

-spec gear(ModelName::atom(), PrototypeName::atom(), Parts::list(assembly()), ModelOptions::term(), PrototypeOptions::term()) -> assembly().
gear(ModelName, PrototypeName, Parts, ModelOptions, PrototypeOptions) ->
    Body = #{},
    Product = erlmachine_gear:gear(Body),
    Model = #model{name=ModelName, product=Product, options=ModelOptions},
    Prototype = #prototype{name=PrototypeName, options=PrototypeOptions},
    Gear = #assembly{model=Model, prototype=Prototype, parts=Parts},
    Gear.

-spec install_model(Assembly::assembly()) ->
                           success(term()) | failure(E::term(), R::term(), Reject::term()).
install_model(Assembly) ->
    Module = model_name(Assembly),
    SN = serial_no(Assembly),
    Options = model_options(Assembly),
    MN = model_no(Assembly),
    PN = part_no(Assembly),
    %% We can check exported functions accordingly to this kind of behaviour; 
    Module:install(SN, MN, PN, Options).

-spec replace_model(Assembly::assembly(), Repair::assembly(), Body::term()) ->
                           success(term()) | failure(E::term(), R::term(), Reject::term()).
replace_model(Assembly, Repair, Body) ->
    Module = model_name(Assembly),
    SN = serial_no(Assembly),
    ID = serial_no(Repair),
    Module:replace(SN, ID, Body).

-spec accept_model(Assembly::assembly(), Criteria::term(), Body::term()) ->
                          success(Report::term(), Release::term()) | failure(E::term(), R::term(), Reject::term()).
accept_model(Assembly, Criteria, Body) ->
    Module = model_name(Assembly),
    SN = serial_no(Assembly),
    Module:accept(SN, Criteria, Body).

-spec uninstall_model(Assembly::assembly(), Reason::term(), Body::term()) ->
                       ok.
uninstall_model(Assembly, Reason, Body) ->
    Module = model_name(Assembly),
    SN = serial_no(Assembly),
    Module:uninstall(SN, Reason, Body).

-spec installed(Assembly::assembly(), Part::assembly()) ->
                       ok.
installed(Assembly, Part) ->
    Module = prototype_name(Assembly),
    SN = serial_no(Assembly),
    Module:installed(SN, Assembly, Part).

-spec replaced(Assembly::assembly(), Part::assembly(), Extension::assembly()) ->
                     ok.
replaced(Assembly, Part, Extension) ->
    Module = prototype_name(Assembly),
    SN = serial_no(Assembly),
    Module:replaced(SN, Assembly, Part, Extension).

-spec accepted(Assembly::assembly(), Part::assembly(), Criteria::term(), Report::term()) ->
                      ok.
accepted(Assembly, Part, Criteria, Report) ->
    Module = prototype_name(Assembly),
    SN = serial_no(Assembly),
    Module:accepted(SN, Assembly, Part, Criteria, Report).

-spec rejected(Assembly::assembly(), Part::assembly(), Criteria::term(), Report::term()) ->
                      ok.
rejected(Assembly, Part, Criteria, Report) ->
    Module = prototype_name(Assembly),
    SN = serial_no(Assembly),
    Module:rejected(SN, Assembly, Part, Criteria, Report).

-spec uninstalled(Assembly::assembly(), Reason::term(), Part::assembly()) ->
                       ok.
uninstalled(Assembly, Reason, Part) ->
    Module = prototype_name(Assembly),
    SN = serial_no(Assembly),
    Module:uninstalled(SN, Assembly, Part, Reason).

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

-spec attach(Parts::list(assembly()), Part::assembly()) -> list(assembly()).
attach(Parts, Part) ->
   lists:reverse([Part|Parts]).

-spec detach(Parts::list(assembly()), ID::serial_no()) -> {assembly(), list(assembly())}.
detach(Parts, ID) ->
    {value, Part, Filtered} = lists:keytake(ID, #assembly.serial_no, Parts),
    {Part, Filtered}.

-spec switch(Parts::list(assembly()), Part::assembly()) -> assembly().
switch(_Parts, Part) ->
    Part. 
 
-spec serial_no(Assembly::assembly()) -> SN::serial_no().
serial_no(Assembly) ->
    SN = Assembly#assembly.serial_no,
    SN.

-spec serial_no(Assembly::assembly(), SN::serial_no()) -> Release::assembly().
serial_no(Assembly, SN) ->
    Release = Assembly#assembly{serial_no=SN},
    Release.

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

-spec part_no(Assembly::assembly(), PN::term()) -> Release::assembly().
part_no(Assembly, PN) ->
    Release = Assembly#assembly{part_no=PN},
    Release.


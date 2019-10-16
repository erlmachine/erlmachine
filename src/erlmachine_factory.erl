-module(erlmachine_factory).

-folder(<<"erlmachine/factory">>).

-steps([serial_no]).

-export([serial_no/1]).

-behaviour(gen_server).

%% API.

-export([start_link/0]).

%% We assume that factory will also provide production of all components and their registration too;
%% My assumption that is factory can be driven from production capacity perspective; 
%% Measurements over manufactures production activity needs to be provided too;

%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-export([gear/3, gear/4, gear/6]).
-export([shaft/3, shaft/4, shaft/6]).
-export([axle/3, axle/4, axle/6]).
-export([gearbox/3, gearbox/5]).

-export([parts/2, mount/2]).

-include("erlmachine_factory.hrl").
-include("erlmachine_system.hrl").
-include("erlmachine_filesystem.hrl").

-type no()::erlmachine_serial_no:no().

-record(conveyor, {assembly::assembly(), passed=[]::list(station()), stations=[]::list(atom())}).

-type conveyor()::#conveyor{}.

-export_type([conveyor/0]).
%% Here are different kind of builders can be provided;
%% For example - YAML builder;
%% But from begining we are going to build directly from code;

%% The main purpouse of the factory is to provide product planing abilities;
%% We can control available capacity of all individual parts;
%% We can utilize different pools for that purpouse;
%% The all managment over thoose capabilities is a warehouse option;

-spec parts(Assembly::assembly(), Parts::list(assembly())) -> assembly().
parts(Assembly, Parts) when is_list(Parts) ->
    Release = erlmachine_assembly:parts(Assembly, Parts),
    Release.

-spec mount(Assembly::assembly(), Parts::list(assembly())) -> assembly().
mount(Assembly, Parts) ->
    MountParts = [erlmachine_assembly:mount(Part, Assembly) || Part <- Parts],
    Release = parts(Assembly, MountParts),
    Release.

-spec spec(GearBox::assembly(), Part::assembly(), Options::list()) -> assembly().
spec(GearBox, Part, Options) ->
    SN = serial_no(Part), 
    Module = erlmachine_assembly:prototype_name(Part),
    Options = erlmachine_assembly:prototype_options(Part),
    Start = {Module, install, [SN, GearBox, Part, Options]},
    Restart = proplists:get_value(restart, Options, permanent),
    Shutdown = proplists:get_value(shutdown, Options, 5000),
    Modules = proplists:get_value(modules, Options, [Module]),
    Spec = erlmachine_assembly:spec(Part),
    InitSpec = Spec#{id => SN, start => Start, restart => Restart, shutdown => Shutdown, modules => Modules},
    Release = erlmachine_assembly:spec(Part, InitSpec),
    Release.

-spec gear(GearBox::assembly(), Model::atom(), ModelOptions::term()) -> Gear::assembly().
gear(GearBox, Model, ModelOptions) ->
    InstallOptions = [],
    gear(GearBox, Model, ModelOptions, InstallOptions).

-spec gear(GearBox::assembly(), Model::atom(), ModelOptions::term(), InstallOptions::list()) -> Gear::assembly().
gear(GearBox, Model, ModelOptions, InstallOptions) ->
    Prototype = gear_base_prototype:name(), 
    PrototypeOptions = [],
    gear(GearBox, Model, Prototype, ModelOptions, PrototypeOptions, InstallOptions).

-spec gear(GearBox::assembly(), Model::atom(), Prototype::atom(), ModelOptions::term(), PrototypeOptions::list(), InstallOptions::list()) -> Gear::assembly().
gear(GearBox, Model, Prototype, ModelOptions, PrototypeOptions, InstallOptions) ->
    Gear = erlmachine_assembly:gear(Model, Prototype, ModelOptions,  [{trap_exit, true}|PrototypeOptions]),
    Release = pass(Gear, [?MODULE]),
    spec(GearBox, Release, InstallOptions).

-spec shaft(GearBox::assembly(), Model::atom(), ModelOptions::term()) -> Shaft::assembly().
shaft(GearBox, Model, ModelOptions) ->
    InstallOptions = [],
    shaft(GearBox, Model, ModelOptions, InstallOptions).

-spec shaft(GearBox::assembly(), Model::atom(), ModelOptions::term(), InstallOptions::list()) -> Shaft::assembly().
shaft(GearBox, Model, ModelOptions, InstallOptions) ->
    Prototype = shaft_base_prototype:name(), 
    PrototypeOptions = [],
    shaft(GearBox, Model, Prototype, ModelOptions, PrototypeOptions, InstallOptions).

-spec shaft(GearBox::assembly(), Model::atom(), Prototype::atom(), ModelOptions::term(), PrototypeOptions::list(), InstallOptions::list()) -> Shaft::assembly().
shaft(GearBox, Model, Prototype, ModelOptions, PrototypeOptions, InstallOptions) ->
    Shaft = erlmachine_assembly:shaft(Model, Prototype, ModelOptions,  [{trap_exit, true}|PrototypeOptions]),
    Release = pass(Shaft, [?MODULE]),
    spec(GearBox, Release, InstallOptions).

-spec axle(GearBox::assembly(), Model::atom(), ModelOptions::term()) -> Axle::assembly().
axle(GearBox, Model, ModelOptions) ->
    InstallOptions = [],
    axle(GearBox, Model, ModelOptions, InstallOptions).

-spec axle(GearBox::assembly(), Model::atom(), ModelOptions::term(), InstallOptions::list()) -> Axle::assembly().
axle(GearBox, Model, ModelOptions, InstallOptions) ->
    Prototype = axle_base_prototype:name(), 
    PrototypeOptions = [],
    axle(GearBox, Model, Prototype, ModelOptions, PrototypeOptions, InstallOptions).

-spec axle(GearBox::assembly(), Model::atom(), Prototype::atom(), ModelOptions::term(), PrototypeOptions::list(), InstallOptions::list()) -> Axle::assembly().
axle(GearBox, Model, Prototype, ModelOptions, PrototypeOptions, InstallOptions) ->
    Axle = erlmachine_assembly:axle(Model, Prototype, ModelOptions, [{intensity, 1}, {period, 5}|PrototypeOptions]),
    Release = pass(Axle, [?MODULE]),
    spec(GearBox, Release, InstallOptions).

-spec gearbox(Model::atom(), ModelOptions::term(), Env::term()) -> GearBox::assembly().
gearbox(Model, ModelOptions, Env) ->
    Prototype = gearbox_base_prototype:name(), 
    PrototypeOptions = [],
    gearbox(Model, Prototype, ModelOptions, PrototypeOptions, Env).

-spec gearbox(Model::atom(), Prototype::atom(), ModelOptions::term(), PrototypeOptions::list(), Env::term()) -> GearBox::assembly().
gearbox(Model, Prototype, ModelOptions, PrototypeOptions, Env) ->
    GearBox = erlmachine_assembly:gearbox(Model, Prototype, ModelOptions, [{intensity, 1}, {period, 5}|PrototypeOptions], Env),
    Release = pass(GearBox, [?MODULE]),
    Release.

-spec pass(Conveyor::conveyor(), Stations::list(atom())) -> conveyor().
pass(Assembly, Stations) ->
    Conveyor = #conveyor{assembly=Assembly, stations=Stations},
    Pass = pipe(Conveyor),
    %% At that point we can store Pass information and provide research over this data;
    Pass#conveyor.assembly.

-spec pipe(Conveyor::conveyor()) -> Pipe::conveyor().
pipe(#conveyor{stations=Stations}=Conveyor) ->
    BuildStations = [erlnachine_assembly_station:station(Name) || Name <- Stations],
    Pipe =
        lists:foldl(
          fun(Station, #conveyor{assembly=Assembly, passed=Passed}=State) ->
                  PassStation = erlmachine_assembly_station:pass(Station, Assembly),
                  Release = erlmachine_assembly_station:output(PassStation),
                  State#conveyor{assembly=Release, passed=[PassStation|Passed]}
          end,
          Conveyor,
          BuildStations
         ),
    #conveyor{passed=Passed} = Pipe,
    Pipe#conveyor{passed=lists:reverse(Passed)}.

%% API.

-record(serial_no, {}).

-spec serial_no(Assembly::assembly()) -> assembly().
serial_no(Assembly) ->
    %% Just default timeout for the first time;
    ID = id(),
    SN = gen_server:call(ID, #serial_no{}),
    PrefixSN = <<"S/N", "-", SN/binary>>,
    Release = erlmachine_assembly:serial_no(Assembly, PrefixSN),
    Release.

id() -> 
    {local, ?MODULE}.

-spec start_link() -> 
                        success(pid()) | ingnore | failure(E::term()).
start_link() ->
    ID = id(),
    gen_server:start_link(ID, ?MODULE, [], []).

%% gen_server.

-record(state, {serial::integer(), no::no(), file::binary()}).

init([]) ->
    %% A folder will be appended, cause attribute is listed above in the module declaration;
    File = <<"erlmachine_factory.serial">>, Serial = erlmachine:read_serial(File),
    No = erlmachine_serial_no:no(Serial),
    {ok, #state{serial=Serial, no=No, file=File}}.

handle_call(#serial_no{}, _From, #state{serial=Serial, no=No}=State) ->
    SN = erlmachine_serial_no:serial_no(No),
    IncSerial = erlmachine:serial(Serial),
    RotateNo = erlmachine_serial_no:no(No, IncSerial),
    {reply, SN, State#state{serial=IncSerial, no=RotateNo}};

handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

handle_cast(_Msg, State) ->	{noreply, State}.

handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, #state{serial=Serial, file=File}) ->
    ok = erlmachine:write_serial(File, Serial),
    ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

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

-export([gear/3, gear/5]).
-export([shaft/3, shaft/5]).
-export([axle/3, axle/5]).
-export([gearbox/4, gearbox/6]).

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

-spec gear(Model::atom(), Parts::list(assembly()), ModelOptions::term()) -> Gear::assembly().
gear(Model, Parts, ModelOptions) ->
    Prototype = gear_base_prototype:name(), 
    PrototypeOptions = [],
    gear(Model, Prototype, Parts, ModelOptions, PrototypeOptions).

-spec gear(Model::atom(), Prototype::atom(), Parts::list(assembly()), ModelOptions::term(), PrototypeOptions::list()) -> Gear::assembly().
gear(Model, Prototype, Parts, ModelOptions, PrototypeOptions) ->
    Gear = erlmachine_assembly:gear(Model, Prototype, Parts, ModelOptions,  [{trap_exit, true}|PrototypeOptions]),
    Release = pass(Gear, [?MODULE]),
    Release.

-spec shaft(Model::atom(), Parts::list(assembly()), ModelOptions::term()) -> Shaft::assembly().
shaft(Model, Parts, ModelOptions) ->
    Prototype = shaft_base_prototype:name(), 
    PrototypeOptions = [],
    shaft(Model, Prototype, Parts, ModelOptions, PrototypeOptions).

-spec shaft(Model::atom(), Prototype::atom(), Parts::list(assembly()), ModelOptions::term(), PrototypeOptions::list()) -> Shaft::assembly().
shaft(Model, Prototype, Parts, ModelOptions, PrototypeOptions) ->
    Shaft = erlmachine_assembly:shaft(Model, Prototype, Parts, ModelOptions,  [{trap_exit, true}|PrototypeOptions]),
    Release = pass(Shaft, [?MODULE]),
    Release.

-spec axle(Model::atom(), Parts::list(assembly()), ModelOptions::term()) -> Axle::assembly().
axle(Model, Parts, ModelOptions) ->
    Prototype = axle_base_prototype:name(), 
    PrototypeOptions = [],
    axle(Model, Prototype, Parts, ModelOptions, PrototypeOptions).

-spec axle(Model::atom(), Prototype::atom(), Parts::list(assembly()), ModelOptions::term(), PrototypeOptions::list()) -> Axle::assembly().
axle(Model, Prototype, Parts, ModelOptions, PrototypeOptions) ->
    Axle = erlmachine_assembly:axle(Model, Prototype, Parts, ModelOptions, [{intensity, 1}, {period, 5}|PrototypeOptions]),
    Release = pass(Axle, [?MODULE]),
    Release.

-spec gearbox(Model::atom(), Parts::list(assembly()), ModelOptions::term(), Env::term()) -> GearBox::assembly().
gearbox(Model, Parts, ModelOptions, Env) ->
    Prototype = gearbox_base_prototype:name(), 
    PrototypeOptions = [],
    gearbox(Model, Prototype, Parts, ModelOptions, PrototypeOptions, Env).

-spec gearbox(Model::atom(), Prototype::atom(), Parts::list(assembly()), ModelOptions::term(), PrototypeOptions::list(), Env::term()) -> GearBox::assembly().
gearbox(Model, Prototype, Parts, ModelOptions, PrototypeOptions, Env) ->
    GearBox = erlmachine_assembly:gearbox(Model, Prototype, Parts, ModelOptions, [{intensity, 1}, {period, 5}|PrototypeOptions], Env),
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

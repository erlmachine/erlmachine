-module(erlmachine_factory).

-folder(<<"erlmachine/factory">>).

-steps([
        model_name,
        model_options,
        prototype_name,
        prototype_options,
        install_options,
        serial_no,
        install_args,
        install_options
       ]).

-behaviour(gen_server).

%% API.

-export([start_link/0]).

-export([
         model_name/2,
         model_options/2,
         prototype_name/2, 
         prototype_options/2,
         serial_no/2,
         install_args/2,
         install_options/2
        ]).

%% We assume that factory will also provide production of all components and their registration too;
%% My assumption is that factory can be driven from production capacity perspective; 
%% Measurements over manufactures production activity needs to be satisfied too;

%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-export([gear/4, gear/5, gear/7]).
-export([shaft/4, shaft/5, shaft/7]).
-export([axle/4, axle/5, axle/7]).
-export([gearbox/4, gearbox/5, gearbox/7]).

-include("erlmachine_factory.hrl").
-include("erlmachine_system.hrl").
-include("erlmachine_filesystem.hrl").

-type no()::erlmachine_serial_no:no().

%% Here are different kind of builders can be provided;
%% For example - YAML builder;
%% But from begining we are going to build directly from code;

%% The main purpouse of the factory is to provide product planing;
%% We can control available capacity of all individual parts;
%% We can utilize different pools for that purpouse;
%% The all managment over thoose capabilities is a warehouse option;

-spec model_name(Assembly::assembly(), Name::atom()) -> assembly().
model_name(Assembly, Name) ->
    Model = erlmachine_assembly:model(),
    Release = erlmachine_assembly:model_name(erlmachine_assembly:model(Assembly, Model), Name),
    Release.

-spec model_options(Assembly::assembly(), Options::list(term())) -> assembly().
model_options(Assembly, Options) ->
    Release = erlmachine_assembly:model_options(Assembly, Options),
    Release.

-spec prototype_name(Assembly::assembly(), Name::atom()) -> assembly().
prototype_name(Assembly, Name) ->
    Prototype = erlmachine_assembly:prototype(),
    Release = erlmachine_assembly:prototype_name(erlmachine_assembly:prototype(Assembly, Prototype), Name),
    Release.

-spec prototype_options(Assembly::assembly(), Options::list(term())) -> assembly().
prototype_options(Assembly, Options) ->
    Release = erlmachine_assembly:prototype_options(Assembly, Options),
    Release.

-spec install_options(Assembly::assembly(), Options::list(term())) -> assembly().
install_options(Assembly, Options) ->
    Spec = erlmachine_assembly:spec(Assembly),
    Restart = proplists:get_value(restart, Options, permanent),
    Shutdown = proplists:get_value(shutdown, Options, 5000),
    Release =  erlmachine_assembly:spec(Assembly, Spec#{restart => Restart, shutdown => Shutdown}),
    Release.

-record(serial_no, {}).

-spec serial_no(Assembly::assembly(), Prefix::binary()) -> assembly().
serial_no(Assembly, Prefix) ->
    %% Just default timeout for the first time;
    ID = id(),
    SN = gen_server:call(ID, #serial_no{}),
    Release = erlmachine_assembly:serial_no(Assembly, <<Prefix/binary, SN/binary>>),
    Release.

-spec install_args(Assembly::assembly(), GearBox::assembly()) -> assembly().
install_args(Assembly, GearBox) ->
    SN = erlmachine_assembly:serial_no(Assembly), 
    Module = erlmachine_assembly:prototype_name(Assembly),
    Options = erlmachine_assembly:prototype_options(Assembly),
    Start = {Module, install, [SN, GearBox, Assembly, Options]},
    Spec = erlmachine_assembly:spec(Assembly),
    Release = erlmachine_assembly:spec(Assembly, Spec#{id => SN, start => Start, modules => [Module]}),
    Release.

%% Mount action can be achived on the install stage;
%%-spec mount(Assembly::assembly(), Parts::list(assembly())) -> assembly().
%%mount(Assembly, Parts) ->
  %%  MountParts = [erlmachine_assembly:mount(Part, Assembly) || Part <- Parts],
   %% Release = parts(Assembly, MountParts),
    %%Release.

-spec gear(GearBox::assembly(), Model::atom(), ModelOptions::term(), Parts::list(assembly())) -> Gear::assembly().
gear(GearBox, Model, ModelOptions, Parts) ->
    InstallOptions = [],
    gear(GearBox, Model, ModelOptions, InstallOptions, Parts).

-spec gear(GearBox::assembly(), Model::atom(), ModelOptions::term(), InstallOptions::list(), Parts::list(assembly())) -> Gear::assembly().
gear(GearBox, Model, ModelOptions, InstallOptions, Parts) ->
    Prototype = gear_base_prototype:name(), 
    PrototypeOptions = [],
    gear(GearBox, Model, Prototype, ModelOptions, PrototypeOptions, InstallOptions, Parts).

-spec gear(GearBox::assembly(), Model::atom(), Prototype::atom(), ModelOptions::term(), PrototypeOptions::list(), InstallOptions::list(), Parts::list(assembly())) -> Gear::assembly().
gear(GearBox, Model, Prototype, ModelOptions, PrototypeOptions, InstallOptions, Parts) ->
    Gear = erlmachine_assembly:gear(Model, Prototype, ModelOptions,  [{trap_exit, true}|PrototypeOptions]),
    Input = [Model, ModelOptions, Prototype, PrototypeOptions, InstallOptions, <<"SN-G-">>, GearBox, #{type => worker}, Parts],
    Release = pass(Gear, ?MODULE, Input),
    Release.

-spec shaft(GearBox::assembly(), Model::atom(), ModelOptions::term(), Parts::list(assembly())) -> Shaft::assembly().
shaft(GearBox, Model, ModelOptions, Parts) ->
    InstallOptions = [],
    shaft(GearBox, Model, ModelOptions, InstallOptions, Parts).

-spec shaft(GearBox::assembly(), Model::atom(), ModelOptions::term(), InstallOptions::list(), Parts::list(assembly())) -> Shaft::assembly().
shaft(GearBox, Model, ModelOptions, InstallOptions, Parts) ->
    Prototype = shaft_base_prototype:name(), 
    PrototypeOptions = [],
    shaft(GearBox, Model, Prototype, ModelOptions, PrototypeOptions, InstallOptions, Parts).

-spec shaft(GearBox::assembly(), Model::atom(), Prototype::atom(), ModelOptions::term(), PrototypeOptions::list(), InstallOptions::list(), Parts::list(assembly())) -> Shaft::assembly().
shaft(GearBox, Model, Prototype, ModelOptions, PrototypeOptions, InstallOptions, Parts) ->
    Shaft = erlmachine_assembly:shaft(Model, Prototype, ModelOptions,  [{trap_exit, true}|PrototypeOptions]),
    Input = [Model, ModelOptions, Prototype, PrototypeOptions, InstallOptions, <<"SN-S-">>, GearBox, #{type => worker}, Parts],
    Release = pass(Shaft, ?MODULE, Input),
    Release.

-spec axle(GearBox::assembly(), Model::atom(), ModelOptions::term(), Parts::list(assembly())) -> Axle::assembly().
axle(GearBox, Model, ModelOptions, Parts) ->
    InstallOptions = [],
    axle(GearBox, Model, ModelOptions, InstallOptions, Parts).

-spec axle(GearBox::assembly(), Model::atom(), ModelOptions::term(), InstallOptions::list(), Parts::list(assembly())) -> Axle::assembly().
axle(GearBox, Model, ModelOptions, InstallOptions, Parts) ->
    Prototype = axle_base_prototype:name(), 
    PrototypeOptions = [],
    axle(GearBox, Model, Prototype, ModelOptions, PrototypeOptions, InstallOptions, Parts).

-spec axle(GearBox::assembly(), Model::atom(), Prototype::atom(), ModelOptions::term(), PrototypeOptions::list(), InstallOptions::list(), Parts::list(assembly())) -> Axle::assembly().
axle(GearBox, Model, Prototype, ModelOptions, PrototypeOptions, InstallOptions, Parts) ->
    Axle = erlmachine_assembly:axle(Model, Prototype, ModelOptions, [{intensity, 1}, {period, 5}|PrototypeOptions]),
    Input = [Model, ModelOptions, Prototype, PrototypeOptions, InstallOptions, <<"SN-A-">>, GearBox, #{type => supervisor}, Parts],
    Release = pass(Axle, ?MODULE, Input),
    Release.

-spec gearbox(Model::atom(), ModelOptions::term(), Env::term(), Parts::list(assembly())) -> GearBox::assembly().
gearbox(Model, ModelOptions, Env, Parts) ->
    InstallOptions = [],
    gearbox(Model, ModelOptions, InstallOptions, Env, Parts).

-spec gearbox(Model::atom(), ModelOptions::term(), InstallOptions::list(), Env::term(), Parts::list(assembly())) -> Axle::assembly().
gearbox(Model, ModelOptions, InstallOptions, Env, Parts) ->
    Prototype = gearbox_base_prototype:name(), 
    PrototypeOptions = [],
    axle(Model, Prototype, ModelOptions, PrototypeOptions, InstallOptions, Env, Parts).

-spec gearbox(Model::atom(), Prototype::atom(), ModelOptions::term(), PrototypeOptions::list(), InstallOptions::list(), Env::term(), Parts::list(assembly())) -> GearBox::assembly().
gearbox(Model, Prototype, ModelOptions, PrototypeOptions, InstallOptions, Env, Parts) ->
    GearBox = erlmachine_assembly:gearbox(Model, Prototype, ModelOptions, [{intensity, 1}, {period, 5}|PrototypeOptions], Env),
    Input = [Model, ModelOptions, Prototype, PrototypeOptions, InstallOptions, <<"SN-GX-">>, #{type => supervisor}, Parts],
    Release = pass(GearBox, ?MODULE, Input),
    Release.

-spec pass(Assembly::assembly(), Name::atom(), Parts::list(term())) -> assembly().
pass(Assembly, Name, Parts) ->
    Station = erlmachine_assembly_station:station(Name, Assembly, Parts),
    Pass = erlmachine_assembly_station:pass(Station),
    %% At that point we can store Pass information and provide research over passed station;
    Release = erlmachine_assembly_station:output(Pass),
    Release.

%% API.

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

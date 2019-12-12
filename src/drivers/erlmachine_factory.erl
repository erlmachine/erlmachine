-module(erlmachine_factory).

-folder(<<"erlmachine/factory">>).

-steps([
        model_name,
        model_options,
        prototype_name,
        prototype_options,
        assembly_options,
        product,
        serial_no
       ]).

-behaviour(gen_server).

%% API.

-export([start_link/0]).

-export([
         model_name/2,
         model_options/2,
         prototype_name/2, 
         prototype_options/2,
         assembly_options/2,
         product/2,
         serial_no/2
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

-export([gear/3, gear/4, gear/6]).
-export([shaft/3, shaft/4, shaft/6]).
-export([axle/3, axle/4, axle/6]).
-export([gearbox/3, gearbox/4, gearbox/6]).

-export([serial_no/0]).

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

-spec model_name(Assembly::assembly(), Name::atom()) -> 
                        assembly().
model_name(Assembly, Name) ->
    Model = erlmachine_assembly:model(),
    Release = erlmachine_assembly:model_name(erlmachine_assembly:model(Assembly, Model), Name),
    Release.

-spec model_options(Assembly::assembly(), Options::list(term())) ->
                           assembly().
model_options(Assembly, Options) ->
    Release = erlmachine_assembly:model_options(Assembly, Options),
    Release.

-spec prototype_name(Assembly::assembly(), Name::atom()) -> 
                            assembly().
prototype_name(Assembly, Name) ->
    Prototype = erlmachine_assembly:prototype(),
    Release = erlmachine_assembly:prototype_name(erlmachine_assembly:prototype(Assembly, Prototype), Name),
    Release.

-spec prototype_options(Assembly::assembly(), Options::list(term())) -> 
                               assembly().
prototype_options(Assembly, Options) ->
    Release = erlmachine_assembly:prototype_options(Assembly, Options),
    Release.

-spec assembly_options(Assembly::assembly(), Options::list()) -> 
                              assembly().
assembly_options(Assembly, Options) ->
    Release = erlmachine_assembly:assembly_options(Assembly, Options),
    Release.

-spec product(Assembly::assembly(), Product::product()) -> 
                     assembly().
product(Assembly, Product) ->
    Release = erlmachine_assembly:product(Assembly, Product),
    Release.

-spec serial_no(Assembly::assembly(), Prefix::binary()) ->
                       assembly().
serial_no(Assembly, Prefix) ->
    SN = erlmachine_factory:serial_no(),
    Release = erlmachine_assembly:serial_no(Assembly, <<Prefix/binary, SN/binary>>),
    Release.

-spec gear(GearBox::assembly(), Model::atom(), ModelOpt::term()) ->
                  Gear::assembly().
gear(GearBox, Model, ModelOpt) ->
    AssemblyOpt = [],
    gear(GearBox, Model, ModelOpt, AssemblyOpt).

-spec gear(GearBox::assembly(), Model::atom(), ModelOpt::term(), AssemblyOpt::list()) -> 
                  Gear::assembly().
gear(GearBox, Model, ModelOpt, AssemblyOpt) ->
    Prot = gear_base_prototype:name(), 
    ProtOpt = [],
    gear(GearBox, Model, Prot, ModelOpt, ProtOpt, AssemblyOpt).

-spec gear(GearBox::assembly(), Model::atom(), Prot::atom(), ModelOpt::term(), ProtOpt::list(), AssemblyOpt::list()) -> 
                  Gear::assembly().
gear(_GearBox, Model, Prot, ModelOpt, ProtOpt, AssemblyOpt) ->
    Body = #{},
    Gear = erlmachine_gear:gear(Body),
    Prefix = <<"SN-G-">>,
    Assembly = erlmachine_assembly:assembly(),
    %% Additional flags
    Input = [Model, ModelOpt, Prot, ProtOpt, [{type, worker}|AssemblyOpt], Gear, Prefix],
    Release = pass(Assembly, ?MODULE, Input),
    Release.

-spec shaft(GearBox::assembly(), Model::atom(), ModelOpt::term()) ->
                   Shaft::assembly().
shaft(GearBox, Model, ModelOpt) ->
    AssemblyOpt = [],
    shaft(GearBox, Model, ModelOpt, AssemblyOpt).

-spec shaft(GearBox::assembly(), Model::atom(), ModelOpt::term(), AssemblyOpt::list()) -> 
                   Shaft::assembly().
shaft(GearBox, Model, ModelOpt, AssemblyOpt) ->
    Prot = shaft_base_prototype:name(), 
    ProtOpt = [],
    shaft(GearBox, Model, Prot, ModelOpt, ProtOpt, AssemblyOpt).

-spec shaft(GearBox::assembly(), Model::atom(), Prot::atom(), ModelOpt::term(), ProtOpt::list(), AssemblyOpt::list()) -> 
                   Shaft::assembly().
shaft(_GearBox, Model, Prot, ModelOpt, ProtOpt, AssemblyOpt) ->
    Body = [],
    Shaft = erlmachine_shaft:shaft(Body),
    Prefix = <<"SN-S-">>,
    Assembly = erlmachine_assembly:assembly(),
    Input = [Model, ModelOpt, Prot, ProtOpt, [{type, worker}|AssemblyOpt], Shaft, Prefix],
    Release = pass(Assembly, ?MODULE, Input),
    Release.

-spec axle(GearBox::assembly(), Model::atom(), ModelOpt::term()) -> 
                  Axle::assembly().
axle(GearBox, Model, ModelOpt) ->
    AssemblyOpt = [],
    axle(GearBox, Model, ModelOpt, AssemblyOpt).

-spec axle(GearBox::assembly(), Model::atom(), ModelOpt::term(), AssemblyOpt::list()) -> 
                  Axle::assembly().
axle(GearBox, Model, ModelOpt, AssemblyOpt) ->
    Prot = axle_base_prototype:name(), 
    ProtOpt = [],
    axle(GearBox, Model, Prot, ModelOpt, ProtOpt, AssemblyOpt).

-spec axle(GearBox::assembly(), Model::atom(), Prot::atom(), ModelOpt::term(), ProtOpt::list(), AssemblyOpt::list()) ->
                  Axle::assembly().
axle(_GearBox, Model, Prot, ModelOpt, ProtOpt, AssemblyOpt) ->
    Body = [],
    Axle = erlmachine_axle:axle(Body),
    Prefix = <<"SN-A-">>,
    Assembly = erlmachine_assembly:assembly(),
    Input = [Model, ModelOpt, Prot, ProtOpt, [{type, supervisor}|AssemblyOpt], Axle, Prefix],
    Release = pass(Assembly, ?MODULE, Input),
    Release.

-spec gearbox(Model::atom(), ModelOpt::term(), Env::term()) ->
                     Axle::assembly().
gearbox(Model, ModelOpt, Env) ->
    AssemblyOpt = [],
    gearbox(Model, ModelOpt, AssemblyOpt, Env).

-spec gearbox(Model::atom(), ModelOpt::term(), AssemblyOpt::list(), Env::term()) ->
                     Axle::assembly().
gearbox(Model, ModelOpt, AssemblyOpt, Env) ->
    Prot = gearbox_base_prototype:name(), 
    ProtOpt = [],
    gearbox(Model, Prot, ModelOpt, ProtOpt, AssemblyOpt, Env).

-spec gearbox(Model::atom(), Prot::atom(), ModelOpt::term(), ProtOpt::list(), AssemblyOpt::list(), Env::term()) -> 
                     GearBox::assembly().
gearbox(Model, Prot, ModelOpt, ProtOpt, AssemblyOpt, Env) ->
    Body = #{}, %% We can consider to store some meta info in body to pass through all building process;
    Schema = digraph:new([acyclic, protected]),
    GearBox = erlmachine_gearbox:gearbox(Body, Env, Schema),
    Prefix = <<"SN-GX-">>,
    Assembly = erlmachine_assembly:assembly(),
    Input = [Model, ModelOpt, Prot, ProtOpt, [{type, supervisor}|AssemblyOpt], GearBox, Prefix],
    Release = pass(Assembly, ?MODULE, Input),
    io:format("~n~p ~p ~p~n",[?MODULE, Schema, self()]),
    Release.

-spec pass(Assembly::assembly(), Name::atom(), Parts::list(term())) ->
                  assembly().
pass(Assembly, Name, Parts) ->
    Station = erlmachine_station:station(Name, Assembly, Parts),
    Pass = erlmachine_station:pass(Station),
    %% At that point we can store Pass information and provide research over passed station;
    Release = erlmachine_station:output(Pass),
    Release.

%% API.

id() -> 
    ?MODULE.

-spec start_link() -> success(pid()) | ingnore | failure(E::term()).
start_link() ->
    ID = id(),
    gen_server:start_link({local, ID}, ?MODULE, [], []).

-record (serial_no, {}).

-spec serial_no() -> serial_no().
serial_no() ->
    %% Just default timeout for the first time;
    ID = id(),
    SN = gen_server:call(ID, #serial_no{}),
    SN.

%% gen_server.

-record(state, {serial::integer(), no::no(), file::binary()}).

init([]) ->
    %% A folder will be appended, cause attribute is listed above in the module declaration;
    File = <<"erlmachine_factory.serial">>, {ok, Serial} = erlmachine:read_serial(File),
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

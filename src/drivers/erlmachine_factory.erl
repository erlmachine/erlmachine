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

-export([accept/2, accept/3]).

-export([accepted/3, rejected/4]).

-export([serial_no/0]).

-include("erlmachine_factory.hrl").
-include("erlmachine_system.hrl").
-include("erlmachine_filesystem.hrl").

-type seed() :: erlmachine:seed().

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
    erlmachine_assembly:model_name(erlmachine_assembly:model(Assembly, Model), Name).

-spec model_options(Assembly::assembly(), Opt::list(term())) ->
                           assembly().
model_options(Assembly, Opt) ->
    erlmachine_assembly:model_options(Assembly, Opt).

-spec prototype_name(Assembly::assembly(), Name::atom()) -> 
                            assembly().
prototype_name(Assembly, Name) ->
    Prototype = erlmachine_assembly:prototype(),
    erlmachine_assembly:prototype_name(erlmachine_assembly:prototype(Assembly, Prototype), Name).

-spec prototype_options(Assembly::assembly(), Opt::list(term())) -> 
                               assembly().
prototype_options(Assembly, Opt) ->
    erlmachine_assembly:prototype_options(Assembly, Opt).

-spec assembly_options(Assembly::assembly(), Opt::list()) -> 
                              assembly().
assembly_options(Assembly, Opt) ->
    erlmachine_assembly:assembly_options(Assembly, Opt).

-spec product(Assembly::assembly(), Product::product()) -> 
                     assembly().
product(Assembly, Product) ->
    erlmachine_assembly:product(Assembly, Product).

-spec serial_no(Assembly::assembly(), Prefix::binary()) ->
                       assembly().
serial_no(Assembly, Prefix) ->
    SN = serial_no(),
    erlmachine_assembly:serial_no(Assembly, <<Prefix/binary, SN/binary>>).

%% I am thinking about two kind of acceptance test;
%% The first one is ability to check prototype with default test models;
%% The second one is acceptance test accordingly to a specific model
%% which can be accomplished by specific implementation;

%% I am thinking about two kind of methods: 
%% SN = warhouse:store(Assembly), warhouse:load(SN) and MN = factory:register(Model), factory:build(MN); 
%% All system elements will be stored by default;
%% We can check the whole list of registered models to provide accept call to each of them;

%% I think about catalog concept for models storage;

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
    %% Additional flags
    Input = [Model, ModelOpt, Prot, ProtOpt, [{type, worker}|AssemblyOpt], Gear, Prefix],
    Assembly = erlmachine_assembly:assembly(),
    pass(Assembly, ?MODULE, Input).

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
    Input = [Model, ModelOpt, Prot, ProtOpt, [{type, worker}|AssemblyOpt], Shaft, Prefix],
    Assembly = erlmachine_assembly:assembly(),
    pass(Assembly, ?MODULE, Input).

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
    Input = [Model, ModelOpt, Prot, ProtOpt, [{type, supervisor}|AssemblyOpt], Axle, Prefix],
    Assembly = erlmachine_assembly:assembly(),
    pass(Assembly, ?MODULE, Input).

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
    Input = [Model, ModelOpt, Prot, ProtOpt, [{type, supervisor}|AssemblyOpt], GearBox, Prefix],
    Assembly = erlmachine_assembly:assembly(),
    pass(Assembly, ?MODULE, Input).

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
    erlmachine:base64url(SN).

%% gen_server.

-record(state, {seed::seed(), serial_no::serial_no()}).

init([]) ->
    %% A folder will be appended, cause attribute is listed above in the module declaration;
    
    {ok, Seed} = erlmachine:read_seed(),
    
    SN = erlmachine_serial_no:serial_no(Seed),
    io:format("~nS: ~p~n",[SN]),
    {ok, #state{seed=Seed, serial_no=SN}}.

handle_call(#serial_no{}, _From, #state{seed=Seed, serial_no=SN}=State) ->
 
    Inc = erlmachine:seed(Seed),
    
    Rotate = erlmachine_serial_no:serial_no(Inc, SN),
    {reply, SN, State#state{seed=Inc, serial_no=Rotate}};

handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

handle_cast(_Msg, State) ->	{noreply, State}.

handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, #state{seed=Seed}) ->
    ok = erlmachine:write_seed(Seed),
    ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

-spec accept(GearBox::assembly(), Criteria::criteria()) -> 
                    success() | failure(E::term(), R::term(), S::term()).
accept(GearBox, Criteria) ->
    SN = erlmachine_assembly:serial_no(GearBox),
    Name = erlmachine_assembly:prototype_name(GearBox),
    
    Result = 
        try 
            erlmachine_assembly:install(GearBox),
            Status = Name:accept(SN, GearBox, Criteria),
            erlmachine_assembly:uninstall(GearBox, normal),
            Status
        catch E:R:S ->
                erlmachine:failure(E, R, S) 
        end,
    Result.

-spec accept(GearBox::assembly(), Assembly::assembly(), Criteria::criteria()) -> 
                    success(term()) | failure(term(), term()).
accept(GearBox, Assembly, Criteria) ->
    SN = erlmachine_assembly:serial_no(Assembly),
    Name = erlmachine_assembly:prototype_name(Assembly),
    
    Result = 
        try 
            erlmachine_assembly:install(GearBox, Assembly),
            Status = Name:accept(SN, GearBox, Assembly, Criteria),
            erlmachine_assembly:uninstall(GearBox, SN, normal),
            Status
        catch E:R:S ->
                erlmachine:failure(E, R, S) 
        end,
    Result.

-spec accepted(GearBox::assembly(), Assembly::assembly(), Criteria::criteria()) -> 
                      ok.
accepted(GearBox, Assembly, Criteria) ->
    SN = erlmachine_assembly:serial_no(GearBox),
    Name = erlmachine_assembly:prototype_name(GearBox),

    Name:accepted(SN, GearBox, Assembly, Criteria).

-spec rejected(GearBox::assembly(), Assembly::assembly(), Criteria::criteria(), Result::term()) -> 
                      ok.
rejected(GearBox, Assembly, Criteria, Result) ->
    SN = erlmachine_assembly:serial_no(GearBox),
    Name = erlmachine_assembly:prototype_name(GearBox),

    Name:rejected(SN, GearBox, Assembly, Criteria, Result).

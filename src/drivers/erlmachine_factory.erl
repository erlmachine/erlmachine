-module(erlmachine_factory).

-folder(<<"erlmachine/factory">>).

-behaviour(gen_server).

%% API.

-export([start_link/0]).

%% We assume that factory will also provide production of all components and their registration too;
%% My assumption is that factory can be driven from production capacity perspective; 
%% Measurements over manufactures production activity needs to be satisfied too;

%% I guess factory needs to have the specialized, predefined parts with reserved serial_no;

%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([handle_continue/2]).
-export([terminate/2]).
-export([code_change/3]).
-export([format_status/2]).

-export([gear/3, gear/4, gear/6]).
-export([shaft/3, shaft/4, shaft/6]).
-export([axle/3, axle/4, axle/6]).
-export([gearbox/3, gearbox/4, gearbox/6]).

-export([accept/2, accept/3, accept/4]).

-export([accepted/3, rejected/4]).

-export([serial_no/0]).

-include("erlmachine_factory.hrl").
-include("erlmachine_system.hrl").
-include("erlmachine_filesystem.hrl").

-type serial() :: erlmachine_serial:serial().

%% Here are different kind of builders can be provided;
%% For example - YAML builder;
%% But from begining we are going to build directly from code;

%% The main purpouse of the factory is to provide product planing;
%% We can control available capacity of all individual parts;
%% We can utilize different pools for that purpouse;

%% I am thinking about two kind of acceptance test;
%% The first one is ability to check prototype with default test models;
%% The second one is acceptance test accordingly to a specific model
%% which can be accomplished by specific implementation;

%% I am thinking about two kind of methods: 
%% SN = warhouse:store(Assembly), warhouse:load(SN) and MN = factory:register(Model), factory:build(MN); 
%% All system elements will be stored by default;
%% We can check the whole list of registered models to provide accept call to each of them;

%% I think about catalog concept for models storage;

-spec gear(GearBox::assembly(), ModelName::atom(), ModelOpt::term()) ->
                  Gear::assembly().
gear(GearBox, ModelName, ModelOpt) ->
    AssemblyOpt = [],
    gear(GearBox, ModelName, ModelOpt, AssemblyOpt).

-spec gear(GearBox::assembly(), ModelName::atom(), ModelOpt::term(), AssemblyOpt::list()) -> 
                  Gear::assembly().
gear(GearBox, ModelName, ModelOpt, AssemblyOpt) ->
    ProtName = gear_base_prototype:name(), 
    ProtOpt = [],
    gear(GearBox, ModelName, ProtName, ModelOpt, ProtOpt, AssemblyOpt).

-spec gear(GearBox::assembly(), ModelName::atom(), ProtName::atom(), ModelOpt::term(), ProtOpt::list(), AssemblyOpt::list()) -> 
                  Gear::assembly().
gear(_GearBox, ModelName, ProtName, ModelOpt, ProtOpt, AssemblyOpt) ->
    Product = erlmachine_gear:gear(#{}),
    Prototype = erlmachine_prototype:prototype(ProtName, ProtOpt, #{}),
    Model = erlmachine_model:model(ModelName, ModelOpt, Prototype, Product),

    SN = <<"G.", (serial_no())/binary>>,

    erlmachine_assembly:assembly(SN, Model, [{type, worker}|AssemblyOpt]).

-spec shaft(GearBox::assembly(), ModelName::atom(), ModelOpt::term()) ->
                   Shaft::assembly().
shaft(GearBox, ModelName, ModelOpt) ->
    AssemblyOpt = [],
    shaft(GearBox, ModelName, ModelOpt, AssemblyOpt).

-spec shaft(GearBox::assembly(), ModelName::atom(), ModelOpt::term(), AssemblyOpt::list()) -> 
                   Shaft::assembly().
shaft(GearBox, ModelName, ModelOpt, AssemblyOpt) ->
    ProtName = shaft_base_prototype:name(), 
    ProtOpt = [],
    shaft(GearBox, ModelName, ProtName, ModelOpt, ProtOpt, AssemblyOpt).

-spec shaft(GearBox::assembly(), ModelName::atom(), ProtName::atom(), ModelOpt::term(), ProtOpt::list(), AssemblyOpt::list()) -> 
                   Shaft::assembly().
shaft(_GearBox, ModelName, ProtName, ModelOpt, ProtOpt, AssemblyOpt) ->
    Product = erlmachine_shaft:shaft([]),
    Prototype = erlmachine_prototype:prototype(ProtName, ProtOpt, #{}),
    Model = erlmachine_model:model(ModelName, ModelOpt, Prototype, Product),

    SN = <<"S.", (serial_no())/binary>>,

    erlmachine_assembly:assembly(SN, Model, [{type, worker}|AssemblyOpt]).

-spec axle(GearBox::assembly(), ModelName::atom(), ModelOpt::term()) -> 
                  Axle::assembly().
axle(GearBox, ModelName, ModelOpt) ->
    AssemblyOpt = [],
    axle(GearBox, ModelName, ModelOpt, AssemblyOpt).

-spec axle(GearBox::assembly(), ModelName::atom(), ModelOpt::term(), AssemblyOpt::list()) -> 
                  Axle::assembly().
axle(GearBox, ModelName, ModelOpt, AssemblyOpt) ->
    ProtName = axle_base_prototype:name(), 
    ProtOpt = [],
    axle(GearBox, ModelName, ProtName, ModelOpt, ProtOpt, AssemblyOpt).

-spec axle(GearBox::assembly(), ModelName::atom(), ProtName::atom(), ModelOpt::term(), ProtOpt::list(), AssemblyOpt::list()) ->
                  Axle::assembly().
axle(_GearBox, ModelName, ProtName, ModelOpt, ProtOpt, AssemblyOpt) ->
    Product = erlmachine_axle:axle([]),
    Prototype = erlmachine_prototype:prototype(ProtName, ProtOpt, #{}),
    Model = erlmachine_model:model(ModelName, ModelOpt, Prototype, Product),

    SN = <<"A.", (serial_no())/binary>>,

    erlmachine_assembly:assembly(SN, Model, [{type, supervisor}|AssemblyOpt]).

-spec gearbox(ModelName::atom(), ModelOpt::term(), Env::term()) ->
                     Axle::assembly().
gearbox(ModelName, ModelOpt, Env) ->
    AssemblyOpt = [],
    gearbox(ModelName, ModelOpt, AssemblyOpt, Env).

-spec gearbox(ModelName::atom(), ModelOpt::term(), AssemblyOpt::list(), Env::term()) ->
                     Axle::assembly().
gearbox(ModelName, ModelOpt, AssemblyOpt, Env) ->
    ProtName = gearbox_base_prototype:name(), 
    ProtOpt = [],
    gearbox(ModelName, ProtName, ModelOpt, ProtOpt, AssemblyOpt, Env).

-spec gearbox(ModelName::atom(), ProtName::atom(), ModelOpt::term(), ProtOpt::list(), AssemblyOpt::list(), Env::term()) -> 
                     GearBox::assembly().
gearbox(ModelName, ProtName, ModelOpt, ProtOpt, AssemblyOpt, Env) ->
    %% We can consider to store some meta info in body to pass through all building process;
    Product = erlmachine_gearbox:gearbox(#{}, Env),
    Prototype = erlmachine_prototype:prototype(ProtName, ProtOpt, #{}),
    Model = erlmachine_model:model(ModelName, ModelOpt, Prototype, Product),

    SN = <<"GX.", (serial_no())/binary>>,

    Assembly = erlmachine_assembly:assembly(SN, Model, [{type, supervisor}|AssemblyOpt]),
    erlmachine_gearbox:master(Assembly).

%% API.

id() -> 
    ?MODULE.

-spec start_link() -> 
                        success(pid()) | ingnore | failure(E::term()).
start_link() ->
    Id = id(),
    gen_server:start_link({local, Id}, ?MODULE, [], []).

-record (serial_no, {}).

-spec serial_no() -> serial_no().
serial_no() ->
    %% Just default timeout for the first time;
    Id = id(),
    SN = gen_server:call(Id, #serial_no{}),
    erlmachine_serial_no:base64url(SN).

%% gen_server.

-record(state, { serial::serial(), serial_no::serial_no() }).
-record(accept, { }).
%% Factory will be responsible for the model's, assemblies, storing and management;
init([]) ->
    %% A folder will be appended, cause attribute is listed above in the module declaration;
    {ok, Serial} = erlmachine_serial:serial_no(),

    SN = erlmachine_serial_no:serial_no(Serial),

    {ok, #state{ serial=Serial, serial_no=SN }}.

handle_call(#serial_no{}, _From, #state{serial=Serial, serial_no=SN}=State) ->
    Inc = erlmachine_serial:inc(Serial),
    Rotate = erlmachine_serial_no:serial_no(Inc, SN),

    {reply, SN, State#state{serial=Inc, serial_no=Rotate}};

handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

handle_cast(_Msg, State) ->	{noreply, State}.

handle_info(_Info, State) ->
	{noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

handle_continue(#accept{}, #state{}=State) ->
    try
        %% TODO acceptance test over SN can be satisfied;
        {noreply, State}
    catch E:R ->
            {stop, {E, R}, State}
    end;
handle_continue(_, State) ->
    {noreply, State}.

terminate(_Reason, #state{serial=Serial}) ->
    ok = erlmachine_serial:serial_no(Serial).

format_status(_Opt, [_PDict, _State]) ->
    [].

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
    accept(GearBox, undefined, Assembly, Criteria).

-spec accept(GearBox::assembly(), Register::term(), Assembly::assembly(), Criteria::criteria()) -> 
                    success(term()) | failure(term(), term()).
accept(GearBox, Register, Assembly, Criteria) ->
    SN = erlmachine_assembly:serial_no(Assembly),
    Name = erlmachine_assembly:prototype_name(Assembly),
    
    Result = 
        try 
            erlmachine_assembly:attach(GearBox, Register, Assembly),
            Status = Name:accept(SN, GearBox, Assembly, Criteria),
            erlmachine_assembly:detach(GearBox, SN),
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

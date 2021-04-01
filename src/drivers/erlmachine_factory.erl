-module(erlmachine_factory).
%% NOTE: The factory is responsible to produce and register component by it's own SN;
%% TODO: The next features:
%% 1. Capacity driven factory. Production planning via allocated pool;
%% 2. Measurements and visualization of production activities in admin panel;
%% 3. Acceptance procedure execution: prototype layer (supported by test model), model (business layer);
%% 4. Registered part_no allocation;

%% NOTE: SN (serial_no) is an runtime in which assigned sequentially and calulcated by non-deterministic way;
%% NOTE: MN (model_no) is an implementation id which is obtained from -vsn(Vsn) tag of the module;
%% NOTE: PN (part_no) is an uptime id which is assigned after factory has restarted;
%% NOTE: The all list can be defined via appropriate module tags: -serial_no(SN), -model_no(MN), -part_no(PN)

%% NOTE: It's encouraged to use datasheet when there is requirement for description

-behaviour(gen_server).

-behaviour(erlmachine_database).
-behaviour(erlmachine_registry).

%% API.

-export([table/0, attributes/0]).

-export([group/0]).

-export([start_link/0]).
-export([start/0]).
-export([process/2]).
-export([stop/0]).

%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).

-export([gear/3, gear/4, gear/5, gear/6]).
-export([shaft/4, shaft/6]).
-export([axle/4, axle/6]).
-export([gearbox/5, gearbox/7]).

-export([assembly/1, assembly/2]).
-export([transmission/1]).

-include("erlmachine_assembly.hrl").
-include("erlmachine_system.hrl").

-type serial_no() :: binary().
-type part_no() :: binary().

-type datasheet() :: erlmachine_datasheet:datasheet().

-type hash() :: binary().
-type uid() :: erlmachine_user:uid().

-type graph() :: erlmachine_graph:graph().

-export_type([serial_no/0, part_no/0]).

%%% erlmachine_db

-spec table() -> atom().
table() ->
    ?MODULE.

-spec attributes() -> [atom()].
attributes() ->
    record_info(fields, ?MODULE).

%%% erlmachine_registry

-spec group() -> atom().
group() ->
    ?MODULE.

-record (erlmachine_factory, { id::atom(), count::integer() }).

id() -> 
    ?MODULE.

-spec start() -> success(pid()) | ingnore | failure(term()).
start() ->
    Id = id(),
    gen_server:start({local, Id}, ?MODULE, [], []).

-spec start_link() -> success(pid()) | ingnore | failure(term()).
start_link() ->
    gen_server:start_link({local, id()}, ?MODULE, [], []).

-record(process, { assembly::assembly(), datasheet::datasheet() }).

-spec process(Assembly::assembly()) ->
                     success(assembly()) | failure(term(), term()).
process(Assembly) ->
    Datasheet = erlmachine_datasheet:new(),
    process(Assembly, Datasheet).

-spec process(Assembly::assembly(), Datasheet::datasheet()) ->
                       success(assembly()) | failure(term(), term()).
process(Assembly, Datasheet) ->
    gen_server:call(id(), #process{ assembly = Assembly, datasheet =  Datasheet }).

-spec stop() -> success().
stop() ->
    gen_server:stop(id()).

%%% gen_server

-record(state, { hash::hash(), serial::serial(), uid::uid() }).

-type state() :: #state{}.

init([]) ->
    Serial = erlmachine_database:update_counter(?MODULE), Hash = erlmachine:guid(Serial),
    UID = erlmachine_user:root(),

    {ok, #state{ hash = Hash, serial = Serial, uid = UID }}.

handle_call(#process{ assembly = Assembly, datasheet = Datasheet }, _From, #state{ hash = Hash } = State) ->
    Steps =
        [
         fun id/2,
         fun serial_no/2,
         fun uid/2,
         fun (A, _) -> erlmachine_assembly:datasheet(A, Datasheet) end,
         fun vsn/2,
         fun publish/2
        ],
    Res = pipe(Steps, Assembly, State),

    <<B1:32, B2:32, B3:32, B4:32>> = Hash, Serial = erlmachine_database:update_counter(?MODULE),
    B5 = erlmachine:phash2({B1, Serial}),
    Rotated = <<(B2 bxor B5):32, (B3 bxor B5):32, (B4 bxor B5):32, B5:32>>,

    {reply, erlmachine:success(Res), State#state{ hash = Rotated, serial = Serial }};

handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
    ok.

-spec pipe(Steps::[function()], Assembly::assembly(), State::state()) -> assembly().
pipe([], Assembly, _State) ->
    Assembly;
pipe([Step|T], Assembly, State) ->
    Rel = Step(Assembly, State),
    pipe(T, Rel, State).


-spec id(Assembly::assembly(), State::state()) -> assembly().
id(Assembly, #state{ serial = Serial }) ->
    erlmachine_assembly:id(Assembly, Serial).

-spec serial_no(Assembly::assembly(), State::state()) -> assembly().
serial_no(Assembly, #state{ hash = Hash }) ->
    SN = erlmachine:base64url(Hash),
    erlmachine_assembly:serial_no(Assembly, SN).

-spec uid(Assembly::assembly(), State::state()) -> assembly().
uid(Assembly, #state{ uid = UID }) ->
    erlmachine_assembly:uid(Assembly, UID).

-spec vsn(Assembly::assembly(), State::state()) -> assembly().
vsn(Assembly, _) ->
    %% TODO: To convert into base64;
    Model = erlmachine_assembly:model(Assembly), ModelName = erlmachine_model:name(Model),
    {_, [ModelVsn]} = erlmachine:attribute(ModelName, 'vsn'),
    ModelRel = erlmachine_model:vsn(Model, ModelVsn),

    Prot = erlmachine_assembly:prototype(Assembly), ProtName = erlmachine_prototype:name(Prot),
    {_, [ProtVsn]} = erlmachine:attribute(ProtName, 'vsn'),
    ProtRel = erlmachine_prototype:vsn(Prot, ProtVsn),

    erlmachine_assembly:prototype(erlmachine_assembly:model(Assembly, ModelRel), ProtRel).

-spec publish(Assembly::assembly(), State::state()) -> assembly().
publish(Assembly, _) ->
    %% NOTE: There is a place where production trackers and ws are notified;
    {ok, _Count} = erlmachine_registry:publish(?MODULE, Assembly),
    Assembly.

%%% Production API

%% TODO To consider a call supported by decorating module as arg;
-spec gear(ModelName::atom(), ModelOpt::[term()], Tags::[term()]) ->
                  assembly().
gear(ModelName, ModelOpt, Tags) ->
    ProtName = erlmachine_prototype_def:name(),
    ProtOpt = [],
    gear(ModelName, ModelOpt, ProtName, ProtOpt, Tags).

-spec gear(ModelName::atom(), ModelOpt::[term()], Tags::[term()], Ext::assembly()) ->
                  assembly().
gear(ModelName, ModelOpt, Tags, Ext) ->
    Gear = gear(ModelName, ModelOpt, Tags),
    erlmachine_assembly:extensions(Gear, [Ext]).

-spec gear(ModelName::atom(), ModelOpt::[term()], ProtName::atom(), ProtOpt::[term()], Tags::[term()]) ->
                  assembly().
gear(ModelName, ModelOpt, ProtName, ProtOpt, Tags) when is_list(ModelOpt),
                                                        is_list(ProtOpt) ->
    Assembly = erlmachine_assembly:new(ModelName, ModelOpt, ProtName, ProtOpt, Tags),
    Gear = erlmachine_gear:new(Assembly), {ok, Rel} = process(Gear),
    Rel.

-spec gear(ModelName::atom(), ModelOpt::[term()], ProtName::atom(), ProtOpt::[term()], Tags::[term()], Ext::assembly()) ->
                  assembly().
gear(ModelName, ModelOpt, ProtName, ProtOpt, Tags, Ext) ->
    Gear = gear(ModelName, ModelOpt, ProtName, ProtOpt, Tags),
    erlmachine_assembly:extensions(Gear, [Ext]).

-spec shaft(ModelName::atom(), ModelOpt::[term()], Tags::[term()], Exts::[assembly()]) ->
                  assembly().
shaft(ModelName, ModelOpt, Tags, Exts) ->
    ProtName = erlmachine_prototype_def:name(),
    ProtOpt = [],
    shaft(ModelName, ModelOpt, ProtName, ProtOpt, Tags, Exts).

-spec shaft(ModelName::atom(), ModelOpt::[term()], ProtName::atom(), ProtOpt::[term()], Tags::[term()], Exts::[assembly()]) ->
                  assembly().
shaft(ModelName, ModelOpt, ProtName, ProtOpt, Tags, Exts) when is_list(ModelOpt),
                                                               is_list(ProtOpt),
                                                               is_list(Exts) ->
    Assembly = erlmachine_assembly:new(ModelName, ModelOpt, ProtName, ProtOpt, Tags),
    Shaft = erlmachine_shaft:new(Assembly), {ok, Rel} = process(Shaft),
    erlmachine_assembly:extensions(Rel, Exts).

-spec axle(ModelName::atom(), ModelOpt::[term()], Tags::[term()], Exts::[assembly()]) ->
                   assembly().
axle(ModelName, ModelOpt, Tags, Exts) ->
    ProtName = erlmachine_sup_prototype_def:name(),
    ProtOpt = [],
    axle(ModelName, ModelOpt, ProtName, ProtOpt, Tags, Exts).

-spec axle(ModelName::atom(), ModelOpt::[term()], ProtName::atom(), ProtOpt::[term()], Tags::[term()], Exts::[assembly()]) ->
                   assembly().
axle(ModelName, ModelOpt, ProtName, ProtOpt, Tags, Exts) when is_list(ModelOpt),
                                                              is_list(ProtOpt),
                                                              is_list(Exts) ->
    Assembly = erlmachine_assembly:new(ModelName, ModelOpt, ProtName, ProtOpt, Tags),
    Axle = erlmachine_axle:new(Assembly), {ok, Rel} = process(Axle),
    erlmachine_assembly:extensions(Rel, Exts).

%% TODO: To provide a build call to generate a schema for convinience;
-spec gearbox(ModelName::atom(), ModelOpt::[term()], Tags::[term()], Exts::[assembly()]) ->
                  assembly().
gearbox(ModelName, ModelOpt, Tags, Exts) ->
    ProtName = erlmachine_sup_prototype_def:name(),
    ProtOpt = [],
    gearbox(ModelName, ModelOpt, ProtName, ProtOpt, Tags, Exts).

-spec gearbox(ModelName::atom(), ModelOpt::[term()], ProtName::atom(), ProtOpt::[term()], Tags::[term()], Exts::[assembly()]) ->
                  assembly().
gearbox(ModelName, ModelOpt, ProtName, ProtOpt, Tags, Exts) when is_list(ModelOpt),
                                                                 is_list(ProtOpt),
                                                                 is_list(Exts) ->
    Assembly = erlmachine_assembly:new(ModelName, ModelOpt, ProtName, ProtOpt, Tags),
    GearBox = erlmachine_gearbox:new(Assembly), {ok, Rel} = process(GearBox),
    erlmachine_assembly:extensions(Rel, Exts).

%% TODO: Transmition should be built here;

-spec assembly(Datasheet::datasheet()) ->
                      assembly().
assembly(Datasheet) ->
    Assembly = erlmachine_assembly:new(), {ok, Rel} = process(Assembly, Datasheet),
    Rel.

-spec assembly(Datasheet::datasheet(), Exts::[assembly()]) ->
                      assembly().
assembly(Datasheet, Exts) when is_list(Exts) ->
    Assembly = assembly(Datasheet),
    erlmachine_assembly:extensions(Assembly, Exts).

-spec transmission(Datasheet::datasheet()) -> graph().
transmission(_Datasheet) ->
    ok.

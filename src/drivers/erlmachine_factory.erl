-module(erlmachine_factory).
%% NOTE: Factory is responsible to produce and register component by it's own SN;

%% TODO: The next features:
%% 1. Capacity driven factory. Production planning via allocated pool;
%% 2. Measurements and visualization of production activities in admin panel;
%% 3. Acceptance procedure between layers: prototype (mocked by test model), model (business layer);

%% NOTE: SN (serial_no) is an runtime id which is assigned sequentially and calulcated via non-deterministic way;
%% NOTE: MN (model_no) is an implementation id which is obtained from -vsn(Vsn) tag of the module;

%% NOTE: It's encouraged to use datasheet when there is a requirement to document produced assembly;

-behaviour(gen_server).

-behaviour(erlmachine_table).
-behaviour(erlmachine_scope).

%% API.

-export([is_factory/1]).

-export([table/0, attributes/0]).

-export([scope/0]).

-export([start_link/0, start/0]).
-export([stop/0]).

-export([init/1]).
-export([handle_call/3, handle_cast/2, handle_info/2]).
-export([terminate/2]).

-export([gear/3, gear/4, gear/5, gear/6]).
-export([shaft/4, shaft/6]).
-export([axle/4, axle/6]).
-export([gearbox/4, gearbox/6]).

-export([assembly/1, assembly/2]).
-export([graph/1]).

-include("erlmachine_assembly.hrl").
-include("erlmachine_template.hrl").
-include("erlmachine_user.hrl").
-include("erlmachine_graph.hrl").
-include("erlmachine_system.hrl").

-callback process(Assembly::assembly(), T::template()) -> assembly().

-type serial_no() :: binary().
-type part_no() :: binary().

-type hash() :: binary().

-export_type([serial_no/0, part_no/0]).

%%% Modules

-spec is_factory(Module::atom()) -> boolean().
is_factory(Module) ->
    lists:member(?MODULE, erlmachine:behaviours(Module)).

-spec factories() -> [module()].
factories() ->
    Modules = erlmachine:modules(),

    [M || M <- Modules, is_factory(M)].

%%% Factories

-spec process(Factories::[module()], Assembly::assembly(), T::template()) ->
                     assembly().
process([], Assembly, _) ->
    Assembly;
process([H|Factories], Assembly, T) ->
    Rel = H:process(Assembly, T),

    process(Factories, Rel, T).

%%% erlmachine_table

-spec table() -> atom().
table() ->
    ?MODULE.

-record (erlmachine_factory, { id::atom(), count::integer() }).

-spec attributes() -> [atom()].
attributes() ->
    record_info(fields, ?MODULE).

%%% erlmachine_scope

-spec scope() -> atom().
scope() ->
    ?MODULE.

%%% API

-record(produce, { assembly::assembly(), template::template() }).

-spec produce(Assembly::assembly()) ->
                     success(assembly()) | failure(term(), term()).
produce(Assembly) ->
    Template = erlmachine_template:new(),

    produce(Assembly, Template).

-spec produce(Assembly::assembly(), T::template()) ->
                     success(assembly()) | failure(term(), term()).
produce(Assembly, T) ->
    Name = name(),
    gen_server:call(Name, #produce{ 'assembly' = Assembly, 'template' = T }).

name() ->
    ?MODULE.

-spec start() -> success(pid()) | ingnore | failure(term()).
start() ->
    Name = name(),
    gen_server:start({local, Name}, ?MODULE, [], []).

-spec start_link() -> success(pid()) | ingnore | failure(term()).
start_link() ->
    Name = name(),
    gen_server:start_link({local, Name}, ?MODULE, [], []).

-spec stop() -> success().
stop() ->
    Name = name(),
    gen_server:stop(Name).

%%% gen_server

-record(state, { hash::hash(), serial::integer(), uid::uid(), factories::[module()]}).

-type state() :: #state{}.

init([]) ->
    Factories = factories(),

    Serial = erlmachine_table:inc(?MODULE), Hash = erlmachine:guid(Serial),
    UID = erlmachine_user:root(),

    erlmachine:success(#state{ hash = Hash, serial = Serial, uid = UID, factories = Factories }).

handle_call(#produce{ 'assembly' = Assembly, 'template' = T }, _From, State) ->
    Hash = hash(State), Factories = factories(State),

    Steps =
        [
         fun id/2,
         fun serial_no/2,
         fun uid/2,
         fun (A, _) -> process(Factories, A, T) end,
         fun vsn/2,
         fun publish/2
        ],
    Res = assemble(Steps, Assembly, State),

    <<B1:32, B2:32, B3:32, B4:32>> = Hash, Serial = erlmachine_table:inc(?MODULE),
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

-spec assemble(Steps::[function()], Assembly::assembly(), State::state()) -> assembly().
assemble([], Assembly, _State) ->
    Assembly;
assemble([Step|T], Assembly, State) ->
    Rel = Step(Assembly, State),
    assemble(T, Rel, State).

-spec id(Assembly::assembly(), State::state()) -> assembly().
id(Assembly, State) ->
    Serial = serial(State), erlmachine_assembly:id(Assembly, Serial).

-spec serial_no(Assembly::assembly(), State::state()) -> assembly().
serial_no(Assembly, State) ->
    Hash = hash(State), SN = erlmachine:base64url(Hash),

    Rel = erlmachine_assembly:serial_no(Assembly, SN),
    erlmachine_assembly:vertex(Rel, SN).

-spec uid(Assembly::assembly(), State::state()) -> assembly().
uid(Assembly, State) ->
    UID = uid(State), erlmachine_assembly:uid(Assembly, UID).

-spec vsn(Assembly::assembly(), State::state()) -> assembly().
vsn(Assembly, _) ->
    %% TODO: To convert into base64;
    Model = erlmachine_assembly:model(Assembly),
    Module = erlmachine_model:module(Model),

    Vsn = erlmachine:vsn(Module), Model2 = erlmachine_model:vsn(Model, Vsn),

    Prot = erlmachine_assembly:prototype(Assembly),
    Module2 = erlmachine_prototype:module(Prot),

    Vsn2 = erlmachine:vsn(Module2), Prot2 = erlmachine_prototype:vsn(Prot, Vsn2),

    erlmachine_assembly:prototype(erlmachine_assembly:model(Assembly, Model2), Prot2).

-spec publish(Assembly::assembly(), State::state()) -> assembly().
publish(Assembly, _) ->
    %% NOTE: There is a place where production trackers and ws are notified;

    {ok, _Count} = erlmachine_scope:publish(?MODULE, _Group = ?MODULE, Assembly),
    Assembly.

%%% Field accessors

-spec factories(State::state()) -> [module()].
factories(State) ->
    State#state.factories.

-spec hash(State::state()) -> hash().
hash(State) ->
    State#state.hash.

-spec serial(State::state()) -> integer().
serial(State) ->
    State#state.serial.

-spec uid(State::state()) -> uid().
uid(State) ->
    State#state.uid.

%%% Production API

-spec gear(Model::atom(), Opt::map(), Env::map()) ->
                  assembly().
gear(Model, Opt, Env) ->
    Prot = erlmachine_prototype_def:name(),
    gear(Model, Opt, Prot, _ProtOpt = #{}, Env).

-spec gear(Model::atom(), Opt::map(), Env::map(), Ext::assembly()) ->
                  assembly().
gear(Model, Opt, Env, Ext) ->
    Gear = gear(Model, Opt, Env),
    erlmachine_assembly:extensions(Gear, [Ext]).

-spec gear(Model::atom(), Opt::map(), Prot::atom(), ProtOpt::map(), Env::map()) ->
                  assembly().
gear(Model, Opt, Prot, ProtOpt, Env) when is_map(Opt),
                                          is_map(ProtOpt) ->
    Assembly = erlmachine_assembly:new(Model, Opt, Prot, ProtOpt, Env),
    Gear = erlmachine_gear:new(Assembly),

    {ok, Rel} = produce(Gear),
    Rel.

-spec gear(Model::atom(), Opt::map(), Prot::atom(), ProtOpt::map(), Env::map(), Ext::assembly()) ->
                  assembly().
gear(Model, Opt, Prot, ProtOpt, Env, Ext) ->
    Gear = gear(Model, Opt, Prot, ProtOpt, Env),
    erlmachine_assembly:extensions(Gear, [Ext]).

-spec shaft(Model::atom(), Opt::map(), Env::map(), Exts::[assembly()]) ->
                  assembly().
shaft(Model, Opt, Env, Exts) ->
    Prot = erlmachine_prototype_def:name(),
    shaft(Model, Opt, Prot, _ProtOpt = #{}, Env, Exts).

-spec shaft(Model::atom(), Opt::map(), Prot::atom(), ProtOpt::map(), Env::map(), Exts::[assembly()]) ->
                   assembly().
shaft(Model, Opt, Prot, ProtOpt, Env, Exts) when is_map(Opt),
                                                 is_map(ProtOpt),
                                                 is_list(Exts) ->
    Assembly = erlmachine_assembly:new(Model, Opt, Prot, ProtOpt, Env),
    Shaft = erlmachine_shaft:new(Assembly),

    {ok, Rel} = produce(Shaft),
    erlmachine_assembly:extensions(Rel, Exts).

-spec axle(Model::atom(), Opt::map(), Env::map(), Exts::[assembly()]) ->
                   assembly().
axle(Model, Opt, Env, Exts) ->
    Prot = erlmachine_sup_prototype_def:name(),
    axle(Model, Opt, Prot, _ProtOpt = #{}, Env, Exts).

-spec axle(Model::atom(), Opt::map(), Prot::atom(), ProtOpt::map(), Env::map(), Exts::[assembly()]) ->
                   assembly().
axle(Model, Opt, Prot, ProtOpt, Env, Exts) when is_map(Opt),
                                                is_map(ProtOpt),
                                                is_list(Exts) ->
    Assembly = erlmachine_assembly:new(Model, Opt, Prot, ProtOpt, Env),
    Axle = erlmachine_axle:new(Assembly),

    {ok, Rel} = produce(Axle),
    erlmachine_assembly:extensions(Rel, Exts).

-spec gearbox(Model::atom(), Opt::map(), Env::map(), Exts::[assembly()]) ->
                  assembly().
gearbox(Model, Opt, Env, Exts) ->
    Prot = erlmachine_sup_prototype_def:name(),
    gearbox(Model, Opt, Prot, _ProtOpt = #{}, Env, Exts).

-spec gearbox(Model::atom(), Opt::map(), Prot::atom(), ProtOpt::map(), Env::map(), Exts::[assembly()]) ->
                  assembly().
gearbox(Model, Opt, Prot, ProtOpt, Env, Exts) when is_map(Opt),
                                                   is_map(ProtOpt),
                                                   is_list(Exts) ->
    Assembly = erlmachine_assembly:new(Model, Opt, Prot, ProtOpt, Env),
    GearBox = erlmachine_gearbox:new(Assembly),

    {ok, Rel} = produce(GearBox),
    erlmachine_assembly:extensions(Rel, Exts).

%%% Template processing

%% Assembly

-spec assembly(T::template()) -> assembly().
assembly(T) ->
    Assembly = erlmachine_assembly:new(), {ok, Rel} = produce(Assembly, T),
    Rel.

-spec assembly(T::template(), Exts::[assembly()]) -> assembly().
assembly(T, Exts) when is_list(Exts) ->
    Assembly = assembly(T),

    erlmachine_assembly:extensions(Assembly, Exts).

%% Graph

-spec vertices(T::template()) -> [term()].
vertices(T) ->
    {ok, Res} = erlmachine_template:find(<<"vertices">>, T),

    _Vertices = maps:to_list(Res).

-spec edges(T::template()) -> [term()].
edges(T) ->
    {ok, Edges} = erlmachine_template:find(<<"edges">>, T),

    [begin [E] = maps:to_list(Edge), E end|| Edge <- Edges].

-spec graph(T::template()) -> assembly().
graph(T) ->
    Graph = erlmachine_graph:new(),

    Vertices = vertices(T), Edges = edges(T),
    [begin A = assembly(D), _ = add_vertex(Graph, V, A) end || {V, D} <- Vertices],

    [_ = add_edge(Graph, V, V2)|| {V, V2} <- Edges],
    Graph.

-spec add_vertex(V::binary(), Assembly::assembly(), Graph::graph()) ->
                         term().
add_vertex(Graph, V, Assembly) ->
    Rel = erlmachine_assembly:vertex(Assembly, V),

    _ = erlmachine_graph:add_vertex(Graph, V, Rel).

-spec add_edge(Graph::graph(), V::binary(), V2::binary() | [binary()]) ->
                       term().
add_edge(Graph, V, V2) when is_list(V2) ->
    [_ = erlmachine_graph:add_edge(Graph, V, Vn, []) || Vn <- V2];
add_edge(Graph, V, V2) ->
    _ = erlmachine_graph:add_edge(Graph, V, V2, []).


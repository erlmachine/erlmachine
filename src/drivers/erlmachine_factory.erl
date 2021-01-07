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

%% API.

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

%% extensions
-export([gear/1, gear/2, gear/3, gear/4, gear/5, gear/6]).
-export([shaft/2, shaft/4, shaft/6]).
-export([axle/2, axle/4, axle/6]).
-export([gearbox/2, gearbox/5, gearbox/7]).

-export([tabname/0]).

-include("erlmachine_assembly.hrl").
-include("erlmachine_system.hrl").

-type serial_no() :: binary().
-type part_no() :: binary().

-type datasheet() :: erlmachine_datasheet:datasheet().

-type hash() :: binary().
-type uid() :: erlmachine_user:uid().
-type serial() :: non_neg_integer().

-export_type([serial_no/0, part_no/0]).

-spec tabname() -> atom().
tabname() ->
    ?MODULE.

-spec update_serial() -> serial().
update_serial() ->
    mnesia:dirty_update_counter(tabname(), ?MODULE, 1).

-record (erlmachine_factory, { id::atom(), count::serial() }).

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

-record(state, { hash::hash(), serial::serial(), uid::uid() }).

-type state() :: #state{}.

init([]) ->
    Attrs = {attributes, record_info(fields, ?MODULE)}, _TabRes = mnesia:create_table(tabname(), [Attrs]),
    Serial = update_serial(),
    Hash = erlmachine:guid(Serial),
    UID = erlmachine_user:root(),
    {ok, #state{ hash = Hash, serial = Serial, uid = UID }}.

handle_call(#process{ assembly = Assembly, datasheet = Datasheet }, _From, #state{ hash = Hash } = State) ->
    I = erlmachine_datasheet:iterator(Datasheet), Next = erlmachine_datasheet:next(I),

    Steps = [fun serial_no/2, fun uid/2, fun(A, _) -> next(A, Next) end, fun vsn/2],
    Res = pipe(Steps, Assembly, State),

    <<B1:32, B2:32, B3:32, B4:32>> = Hash,
    B5 = erlmachine:phash2({B1, update_serial()}),
    Rotated = <<(B2 bxor B5):32, (B3 bxor B5):32, (B4 bxor B5):32, B5:32>>,

    {reply, erlmachine:success(Res), State#state{ hash = Rotated }};

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

-spec serial_no(Assembly::assembly(), State::state()) -> assembly().
serial_no(Assembly, #state{ hash = Hash }) ->
    Name = erlmachine_assembly:name(Assembly),
    SN =  <<(Name:prefix())/binary, "-", (erlmachine:base64url(Hash))/binary>>,
    Rel = erlmachine_assembly:serial_no(Assembly, SN),
    erlmachine_assembly:vertex(Rel, SN).

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

%%%===================================================================
%%% Datasheet processing
%%%===================================================================

-spec next(Assembly::assembly(), none | {Key::binary(), Value::term(), I::term()}) ->
                  assembly().
next(Assembly, none) ->
    Assembly;

next(Assembly, {<<"serial_no">>, SN, I}) ->
    Rel = erlmachine_assembly:serial_no(Assembly, SN),
    next(Rel, erlmachine_datasheet:next(I));

next(Assembly, {<<"body">>, Body, I}) ->
    Rel = erlmachine_assembly:body(Assembly, Body),
    next(Rel, erlmachine_datasheet:next(I));

next(Assembly, {<<"model_no">>, MN, I}) ->
    Rel = erlmachine_assembly:model_no(Assembly, MN),
    next(Rel, erlmachine_datasheet:next(I));

next(Assembly, {<<"socket">>, Socket, I}) ->
    Rel = erlmachine_assembly:socket(Assembly, Socket),
    next(Rel, erlmachine_datasheet:next(I));

next(Assembly, {<<"model">>, Model, I}) ->
    {ok, Name} = erlmachine_datasheet:find(<<"name">>, Model), Module = binary_to_existing_atom(Name, utf8),
    {ok, Opt} = erlmachine_datasheet:find(<<"options">>, Model),

    Rel = erlmachine_assembly:model(Assembly, erlmachine_model:model(Module, Opt)),
    next(Rel, erlmachine_datasheet:next(I));

next(Assembly, {<<"prototype">>, Prot, I}) ->
    {ok, Name} = erlmachine_datasheet:find(<<"name">>, Prot), Module = binary_to_existing_atom(Name, utf8),
    {ok, Opt} = erlmachine_datasheet:find(<<"options">>, Prot),

    Rel = erlmachine_assembly:prototype(Assembly, erlmachine_prototype:prototype(Module, Opt)),
    next(Rel, erlmachine_datasheet:next(I));

next(Assembly, {<<"tags">>, Tags, I}) ->
    Rel = erlmachine_assembly:tags(Assembly, Tags),
    next(Rel, erlmachine_datasheet:next(I));

next(Assembly, {<<"vertex">>, V, I}) ->
    Rel = erlmachine_assembly:vertex(Assembly, V),
    next(Rel, erlmachine_datasheet:next(I));

next(Assembly, {<<"part_no">>, PN, I}) ->
    Rel = erlmachine_assembly:part_no(Assembly, PN),
    next(Rel, erlmachine_datasheet:next(I));

next(Assembly, {<<"env">>, Env, I}) ->
    Rel = erlmachine_assembly:env(Assembly, Env),
    next(Rel, erlmachine_datasheet:next(I));

next(Assembly, {<<"description">>, Desc, I}) ->
    Rel = erlmachine_assembly:description(Assembly, Desc),
    next(Rel, erlmachine_datasheet:next(I));

next(Assembly, {_, _, I}) ->
    next(Assembly, erlmachine_datasheet:next(I)).

%%%===================================================================
%%% Extensions
%%%===================================================================
%% TODO: Factory is responsible to support datasheets; erlmachine_datasheet shouldn't know about assembly;
-spec gear(Datasheet::datasheet()) -> 
                  assembly().
gear(Datasheet) ->
    Gear = erlmachine_gear:gear(),

    {ok, Rel} = process(Gear, Datasheet),
    Rel.

-spec gear(Datasheet::datasheet(), Ext::assembly()) ->
                  assembly().
gear(Datasheet, Ext) ->
    Gear = gear(Datasheet),
    erlmachine_assembly:extensions(Gear, [Ext]).

-spec gear(ModelName::atom(), ModelOpt::[term()], Tags::[term()]) ->
                  assembly().
gear(ModelName, ModelOpt, Tags) ->
    ProtName = erlmachine_worker_prototype_default:name(),
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
    Gear = erlmachine_gear:gear(),

    Prot = erlmachine_prototype:prototype(ProtName, ProtOpt),
    Model = erlmachine_model:model(ModelName, ModelOpt),

    Assembly = erlmachine_assembly:prototype(erlmachine_assembly:model(Gear, Model), Prot),
    {ok, Rel} = process(erlmachine_assembly:tags(Assembly, Tags)),
    Rel.

-spec gear(ModelName::atom(), ModelOpt::[term()], ProtName::atom(), ProtOpt::[term()], Tags::[term()], Ext::assembly()) ->
                  assembly().
gear(ModelName, ModelOpt, ProtName, ProtOpt, Tags, Ext) ->
    Gear = gear(ModelName, ModelOpt, ProtName, ProtOpt, Tags),
    erlmachine_assembly:extensions(Gear, [Ext]).

-spec shaft(Datasheet::datasheet(), Exts::[assembly()]) ->
                  assembly().
shaft(Datasheet, Exts) when is_list(Exts) ->
    Shaft = erlmachine_shaft:shaft(),

    {ok, Rel} = process(Shaft, Datasheet),
    erlmachine_assembly:extensions(Rel, Exts).

-spec shaft(ModelName::atom(), ModelOpt::[term()], Tags::[term()], Exts::[assembly()]) ->
                  assembly().
shaft(ModelName, ModelOpt, Tags, Exts) ->
    ProtName = erlmachine_worker_prototype_default:name(),
    ProtOpt = [],
    shaft(ModelName, ModelOpt, ProtName, ProtOpt, Tags, Exts).

-spec shaft(ModelName::atom(), ModelOpt::[term()], ProtName::atom(), ProtOpt::[term()], Tags::[term()], Exts::[assembly()]) ->
                  assembly().
shaft(ModelName, ModelOpt, ProtName, ProtOpt, Tags, Exts) when is_list(ModelOpt),
                                                               is_list(ProtOpt),
                                                               is_list(Exts) ->
    Shaft = erlmachine_shaft:shaft(),

    Prot = erlmachine_prototype:prototype(ProtName, ProtOpt),
    Model = erlmachine_model:model(ModelName, ModelOpt),

    Assembly = erlmachine_assembly:prototype(erlmachine_assembly:model(Shaft, Model), Prot),
    {ok, Rel} = process(erlmachine_assembly:tags(Assembly, Tags)),
    erlmachine_assembly:extensions(Rel, Exts).

-spec axle(Datasheet::datasheet(), Exts::[assembly()]) -> 
                   assembly().
axle(Datasheet, Exts) ->
    Axle = erlmachine_axle:axle(),

    {ok, Rel} = process(Axle, Datasheet),
    erlmachine_assembly:extensions(Rel, Exts).

-spec axle(ModelName::atom(), ModelOpt::[term()], Tags::[term()], Exts::[assembly()]) ->
                   assembly().
axle(ModelName, ModelOpt, Tags, Exts) ->
    ProtName = erlmachine_supervisor_prototype_default:name(),
    ProtOpt = [],
    axle(ModelName, ModelOpt, ProtName, ProtOpt, Tags, Exts).

-spec axle(ModelName::atom(), ModelOpt::[term()], ProtName::atom(), ProtOpt::[term()], Tags::[term()], Exts::[assembly()]) ->
                   assembly().
axle(ModelName, ModelOpt, ProtName, ProtOpt, Tags, Exts) when is_list(ModelOpt),
                                                              is_list(ProtOpt),
                                                              is_list(Exts) ->
    Axle = erlmachine_axle:axle(),

    Prot = erlmachine_prototype:prototype(ProtName, ProtOpt),
    Model = erlmachine_model:model(ModelName, ModelOpt),

    Assembly = erlmachine_assembly:prototype(erlmachine_assembly:model(Axle, Model), Prot),
    {ok, Rel} = process(erlmachine_assembly:tags(Assembly, Tags)),
    erlmachine_assembly:extensions(Rel, Exts).

%% Gearbox should be responsible to pass env context through the each model;
%% Each extension inherites this context as execution scope;
-spec gearbox(Datasheet::datasheet(), Exts::[assembly()]) ->
                  assembly().
gearbox(Datasheet, Exts) ->
    GearBox = erlmachine_gearbox:gearbox(),

    {ok, Rel} = process(GearBox, Datasheet),
    schema(erlmachine_assembly:extensions(Rel, Exts)).

-spec gearbox(ModelName::atom(), ModelOpt::[term()], Env::map(), Tags::[term()], Exts::[assembly()]) ->
                  assembly().
gearbox(ModelName, ModelOpt, Env, Tags, Exts) ->
    ProtName = erlmachine_supervisor_prototype_default:name(),
    ProtOpt = [],
    gearbox(ModelName, ModelOpt, ProtName, ProtOpt, Env, Tags, Exts).

-spec gearbox(ModelName::atom(), ModelOpt::[term()], ProtName::atom(), ProtOpt::[term()], Env::map(), Tags::[term()], Exts::[assembly()]) ->
                  assembly().
gearbox(ModelName, ModelOpt, ProtName, ProtOpt, Env, Tags, Exts) when is_list(ModelOpt),
                                                                      is_list(ProtOpt),
                                                                      is_map(Env),
                                                                      is_list(Exts) ->
    GearBox = erlmachine_gearbox:gearbox(),

    Prot = erlmachine_prototype:prototype(ProtName, ProtOpt),
    Model = erlmachine_model:model(ModelName, ModelOpt),

    Assembly = erlmachine_assembly:prototype(erlmachine_assembly:model(GearBox, Model), Prot),
    {ok, Rel} = process(erlmachine_assembly:tags(erlmachine_assembly:env(Assembly, Env), Tags)),
    schema(erlmachine_assembly:extensions(Rel, Exts)).

-spec schema(GearBox::assembly()) ->
                    assembly().
schema(GearBox) ->
    Root = erlmachine:serial_no(GearBox), Schema = erlmachine_schema:new(Root),
    Rel = erlmachine_assembly:schema(GearBox, Schema),
    ok = add(Schema, [Rel]),
    Rel.

add(_Schema, []) ->
    ok;
add(Schema, [H|T]) ->
    add_vertex(Schema, H),
    Exts = erlmachine_assembly:extensions(H),
    add(Schema, Exts), [add_edge(Schema, H, Ext) || Ext <- Exts],
    add(Schema, T).

add_vertex(Schema, Assembly) ->
    Rel = erlmachine_assembly:extensions(Assembly, []), V = erlmachine_assembly:vertex(Rel),
    erlmachine_schema:add_vertex(Schema, V, Rel),
    ok.

add_edge(Schema, Assembly, Ext) ->
    V1 = erlmachine_assembly:vertex(Assembly), V2 = erlmachine_assembly:vertex(Ext),
    erlmachine_schema:add_edge(Schema, V1, V2, []),
    ok.


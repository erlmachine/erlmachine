-module(erlmachine_factory).
%% NOTE: The factory is responsible to produce and register component by it's own SN;
%% TODO: The next features:
%% 1. Capacity driven factory. Production planning via allocated pool;
%% 2. Measurements and visualization of production activities in admin panel;
%% 3. Acceptance procedure execution: prototype layer (supported by test model), model (business layer);
%% 4. Registered part_no allocation;

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

-export_type([serial_no/0, part_no/0]).

-spec tabname() -> atom().
tabname() ->
    ?MODULE.

-spec update_counter() -> non_neg_integer().
update_counter() ->
    mnesia:dirty_update_counter(tabname(), ?MODULE, 1).

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

-record(state, { hash::binary() }).

init([]) ->
    Attributes = {attributes, record_info(fields, ?MODULE)},
    _TabRes = mnesia:create_table(tabname(), [Attributes]),
    Hash = erlmachine:guid(update_counter()),
    {ok, #state{ hash = Hash }}.

handle_call(#process{ assembly = Assembly, datasheet = Datasheet }, _From, #state{ hash = Hash } = State) ->
    Name = erlmachine_assembly:name(Assembly),
    SN =  <<(Name:prefix())/binary, (erlmachine:base64url(Hash))/binary>>,
    MN = erlmachine:phash2(Assembly),

    Rel = erlmachine_assembly:model_no(erlmachine_assembly:serial_no(Assembly, SN), MN),

    I = erlmachine_datasheet:iterator(Datasheet), Next = erlmachine_datasheet:next(I),

    <<B1:32, B2:32, B3:32, B4:32>> = Hash,
    B5 = erlmachine:phash2({B1, update_counter()}),
    Rotated = <<(B2 bxor B5):32, (B3 bxor B5):32, (B4 bxor B5):32, B5:32>>,

    {reply, erlmachine:success(next(Rel, Next)), State#state{ hash = Rotated }};

handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
    ok.

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

next(Assembly, {<<"label">>, Label, I}) ->
    Rel = erlmachine_assembly:label(Assembly, Label),
    next(Rel, erlmachine_datasheet:next(I));

next(Assembly, {<<"part_no">>, PN, I}) ->
    Rel = erlmachine_assembly:part_no(Assembly, PN),
    next(Rel, erlmachine_datasheet:next(I));

next(Assembly, {<<"env">>, Env, I}) ->
    Rel = erlmachine_assembly:env(Assembly, Env),
    next(Rel, erlmachine_datasheet:next(I));

next(Assembly, {<<"desc">>, Desc, I}) ->
    Rel = erlmachine_assembly:desc(Assembly, Desc),
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

-spec gear(ModelName::atom(), ModelOpt::term(), Desc::binary()) ->
                  assembly().
gear(ModelName, ModelOpt, Desc) ->
    ProtName = erlmachine_worker_prototype_default:name(),
    ProtOpt = [],
    gear(ModelName, ModelOpt, ProtName, ProtOpt, Desc).

-spec gear(ModelName::atom(), ModelOpt::term(), Desc::binary(), Ext::assembly()) ->
                  assembly().
gear(ModelName, ModelOpt, Desc, Ext) ->
    Gear = gear(ModelName, ModelOpt, Desc),
    erlmachine_assembly:extensions(Gear, [Ext]).

-spec gear(ModelName::atom(), ModelOpt::term(), ProtName::atom(), ProtOpt::list(), Desc::binary()) -> 
                  assembly().
gear(ModelName, ModelOpt, ProtName, ProtOpt, Desc) ->
    Gear = erlmachine_gear:gear(),

    Prot = erlmachine_prototype:prototype(ProtName, ProtOpt),
    Model = erlmachine_model:model(ModelName, ModelOpt),

    Assembly = erlmachine_assembly:prototype(erlmachine_assembly:model(Gear, Model), Prot),
    {ok, Rel} = process(erlmachine_assembly:desc(Assembly, Desc)),
    Rel.

-spec gear(ModelName::atom(), ModelOpt::term(), ProtName::atom(), ProtOpt::list(), Desc::binary(), Ext::assembly()) ->
                  assembly().
gear(ModelName, ModelOpt, ProtName, ProtOpt, Desc, Ext) ->
    Gear = gear(ModelName, ModelOpt, ProtName, ProtOpt, Desc),
    erlmachine_assembly:extensions(Gear, [Ext]).

-spec shaft(Datasheet::datasheet(), Exts::list()) ->
                  assembly().
shaft(Datasheet, Exts) when is_list(Exts) ->
    Shaft = erlmachine_shaft:shaft(),

    {ok, Rel} = process(Shaft, Datasheet),
    erlmachine_assembly:extensions(Rel, Exts).

-spec shaft(ModelName::atom(), ModelOpt::term(), Desc::binary(), Exts::list()) ->
                  assembly().
shaft(ModelName, ModelOpt, Desc, Exts) when is_list(Exts) ->
    ProtName = erlmachine_worker_prototype_default:name(),
    ProtOpt = [],
    shaft(ModelName, ModelOpt, ProtName, ProtOpt, Desc, Exts).

-spec shaft(ModelName::atom(), ModelOpt::term(), ProtName::atom(), ProtOpt::list(), Desc::binary(), Exts::list()) ->
                  assembly().
shaft(ModelName, ModelOpt, ProtName, ProtOpt, Desc, Exts) when is_list(Exts) ->
    Shaft = erlmachine_shaft:shaft(),

    Prot = erlmachine_prototype:prototype(ProtName, ProtOpt),
    Model = erlmachine_model:model(ModelName, ModelOpt),

    Assembly = erlmachine_assembly:prototype(erlmachine_assembly:model(Shaft, Model), Prot),
    {ok, Rel} = process(erlmachine_assembly:desc(Assembly, Desc)),
    erlmachine_assembly:extensions(Rel, Exts).

-spec axle(Datasheet::datasheet(), Exts::list()) -> 
                   assembly().
axle(Datasheet, Exts) ->
    Axle = erlmachine_axle:axle(),

    {ok, Rel} = process(Axle, Datasheet),
    erlmachine_assembly:extensions(Rel, Exts).

-spec axle(ModelName::atom(), ModelOpt::term(), Desc::binary(), Exts::list()) ->
                   assembly().
axle(ModelName, ModelOpt, Desc, Exts) when is_list(Exts) ->
    ProtName = erlmachine_supervisor_prototype_default:name(),
    ProtOpt = [],
    axle(ModelName, ModelOpt, ProtName, ProtOpt, Desc, Exts).

-spec axle(ModelName::atom(), ModelOpt::term(), ProtName::atom(), ProtOpt::list(), Desc::binary(), Exts::list()) -> 
                   assembly().
axle(ModelName, ModelOpt, ProtName, ProtOpt, Desc, Exts) when is_list(Exts) ->
    Axle = erlmachine_axle:axle(),

    Prot = erlmachine_prototype:prototype(ProtName, ProtOpt),
    Model = erlmachine_model:model(ModelName, ModelOpt),

    Assembly = erlmachine_assembly:prototype(erlmachine_assembly:model(Axle, Model), Prot),
    {ok, Rel} = process(erlmachine_assembly:desc(Assembly, Desc)),
    erlmachine_assembly:extensions(Rel, Exts).

%% Gearbox should be responsible to pass env context through the each model;
%% Each extension inherites this context as execution scope;
-spec gearbox(Datasheet::datasheet(), Exts::list()) ->
                  assembly().
gearbox(Datasheet, Exts) ->
    GearBox = erlmachine_gearbox:gearbox(),

    {ok, Rel} = process(GearBox, Datasheet),
    erlmachine_assembly:extensions(Rel, Exts).

-spec gearbox(ModelName::atom(), ModelOpt::term(), Env::term(), Desc::binary(), Exts::list()) ->
                  assembly().
gearbox(ModelName, ModelOpt, Env, Desc, Exts) when is_list(Exts) ->
    ProtName = erlmachine_supervisor_prototype_default:name(),
    ProtOpt = [],
    gearbox(ModelName, ModelOpt, ProtName, ProtOpt, Env, Desc, Exts).

-spec gearbox(ModelName::atom(), ModelOpt::term(), ProtName::atom(), ProtOpt::list(), Env::term(), Desc::binary(), Exts::list()) -> 
                  assembly().
gearbox(ModelName, ModelOpt, ProtName, ProtOpt, Env, Desc, Exts) when is_list(Exts) ->
    GearBox = erlmachine_gearbox:gearbox(),

    Prot = erlmachine_prototype:prototype(ProtName, ProtOpt),
    Model = erlmachine_model:model(ModelName, ModelOpt),

    Assembly = erlmachine_assembly:prototype(erlmachine_assembly:model(GearBox, Model), Prot),
    {ok, Rel} = process(erlmachine_assembly:desc(erlmachine_assembly:env(Assembly, Env), Desc)),
    erlmachine_assembly:extensions(Rel, Exts).

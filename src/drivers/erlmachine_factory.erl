-module(erlmachine_factory).

-folder(<<"erlmachine/factory">>).

-behaviour(gen_server).

%% API.

-export([start_link/0]).

%% We assume that factory will also provide production of all components and their registration too;
%% My assumption is that factory can be driven from production capacity perspective;
%% Measurements over manufactures production activities has to be satisfied too;

%% Factory itself consists of prebuild parts (without build call and assigned serial_no);

%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).

-export([gear/2, gear/3, gear/4, gear/5]).
-export([shaft/3, shaft/5]).
-export([axle/3, axle/5]).
-export([gearbox/4, gearbox/6]).

-export([serial_no/1]).

-include("erlmachine_factory.hrl").
-include("erlmachine_system.hrl").

%% The main purpouse of the factory is to provide production planing;
%% We can control available capacity of the all individual parts;
%% We can utilize different kind of pools for that purpouse;

%% I think about two the most important cases for acceptance procedure:
%% The first one is ability to check prototype with default test model;
%% And the next one is an acceptance test within a specific model implementation;

%% TODO: To implement capacity limits over production cycle;

%% TODO: To provide ability to override default prototypes;
-spec gear(Name::atom(), Opt::term()) -> 
                  assembly().
gear(Name, Opt) ->
    ProtName = erlmachine_worker_sample_prototype:name(), ProtOpt = [],
    gear(Name, Opt, ProtName, ProtOpt).

-spec gear(Name::atom(), Opt::term(), Ext::assembly()) -> 
                  assembly().
gear(Name, Opt, Ext) when is_record(Ext, assembly) ->
    Gear = gear(Name, Opt),
    erlmachine_assembly:extensions(Gear, [Ext]).

-spec gear(Name::atom(), Opt::term(), ProtName::atom(), ProtOpt::list()) -> 
                  assembly().
gear(Name, Opt, ProtName, ProtOpt) ->
    Prot = erlmachine_prototype:prototype(ProtName, ProtOpt),
    Model = erlmachine_model:model(Name, Opt, Prot),
    Body = #{},
    Gear = erlmachine_gear:gear(Body, Model),
    serial_no(Gear).

%% TODO To think about test env param;
-spec gear(Name::atom(), Opt::term(), ProtName::atom(), ProtOpt::list(), Ext::assembly()) -> 
                  assembly().
gear(Name, Opt, ProtName, ProtOpt, Ext) when is_record(Ext, assembly) ->
    Gear = gear(Name, Opt, ProtName, ProtOpt),
    erlmachine_assembly:extensions(Gear, [Ext]).

-spec shaft(Name::atom(), Opt::term(), Exts::list()) -> 
                  assembly().
shaft(Name, Opt, Exts) when is_list(Exts) ->
    ProtName = erlmachine_worker_sample_prototype:name(), ProtOpt = [],
    shaft(Name, Opt, ProtName, ProtOpt, Exts).

-spec shaft(Name::atom(), Opt::term(), ProtName::atom(), ProtOpt::list(), Exts::list()) ->
                  assembly().
shaft(Name, Opt, ProtName, ProtOpt, Exts) when is_list(Exts) ->
    Prot = erlmachine_prototype:prototype(ProtName, ProtOpt),
    Model = erlmachine_model:model(Name, Opt, Prot),
    Body = [],
    Shaft = serial_no(erlmachine_shaft:shaft(Body, Model)),
    erlmachine_assembly:extensions(Shaft, Exts).

-spec axle(Name::atom(), Opt::term(), Exts::list()) ->
                   assembly().
axle(Name, Opt, Exts) when is_list(Exts) ->
    ProtName = erlmachine_supervisor_sample_prototype:name(), ProtOpt = [],
    axle(Name, Opt, ProtName, ProtOpt, Exts).

-spec axle(Name::atom(), Opt::term(), ProtName::atom(), ProtOpt::list(), Exts::list()) -> 
                   assembly().
axle(Name, Opt, ProtName, ProtOpt, Exts) when is_list(Exts) ->
    Prot = erlmachine_prototype:prototype(ProtName, ProtOpt),
    Model = erlmachine_model:model(Name, Opt, Prot),
    Body = [],
    Axle = serial_no(erlmachine_axle:axle(Body, Model)),
    erlmachine_assembly:extensions(Axle, Exts).

%% Gearbox should be responsible to pass env context through the each model;
%% Each extension inherites this context as execution scope;
-spec gearbox(Name::atom(), Opt::term(), Env::term(), Exts::list()) -> 
                  assembly().
gearbox(Name, Opt, Env, Exts) when is_list(Exts) ->
    ProtName = erlmachine_supervisor_sample_prototype:name(), ProtOpt = [],
    gearbox(Name, Opt, ProtName, ProtOpt, Env, Exts).

-spec gearbox(Name::atom(), Opt::term(), ProtName::atom(), ProtOpt::list(), Env::term(), Exts::list()) -> 
                  assembly().
gearbox(Name, Opt, ProtName, ProtOpt, Env, Exts) when is_list(Exts) ->
    Prot = erlmachine_prototype:prototype(ProtName, ProtOpt),
    Model = erlmachine_model:model(Name, Opt, Prot),
    Schema = erlmachine_assembly:schema(), Body = [],
    GearBox = serial_no(erlmachine_gearbox:gearbox(Schema, Body, Model, Env)),
    erlmachine_assembly:extensions(GearBox, Exts).

%% API.

id() -> 
    ?MODULE.

-spec start_link() ->
                        success(pid()) | ingnore | failure(term()).
start_link() ->
    Id = id(),
    gen_server:start_link({local, Id}, ?MODULE, [], []).

-record(build, { assembly::assembly() }).

-spec build(Assembly::assembly()) ->
                  success(term()) | failure(term(), term()).
build(Assembly) ->
    gen_server:call(id(), #build{ assembly=Assembly }).

%% gen_server.

-record(state, { }).

init([]) ->
    {ok, #state{}}.

handle_call(#build{ assembly=Assembly }, From, #state{ gearbox=GearBox }=State) ->
    Command = erlmachine:command(#{}, build, Assembly),

    erlmachine_gearbox:rotate(GearBox, 'build', erlmachine:request_reply(Command, From)),
    {noreply, State};

handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
	{noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

%% TODO:

-type serial()::integer().

-record (serial, { id::atom(), count::integer() }).

%erlmachine:md5(SN).

-spec install(Label::term(), Id::serial_no(), State::map(), Opt::term(), Env::list()) ->
                     success(term()) | failure(term(), term(), term()).
install(_, _, State, _, _) ->
    _TabRes = mnesia:create_table(tabname(), [attributes(attributes()), record_name(record_name())]),

    {ok, State}.

-spec rotate(Label::term(), Motion::map(), State::map()) ->
                     success(term(), term()) | failure(term(), term(), term()).
rotate(_, Motion, State) ->
    Body = erlmachine:body(Motion),
    try
        SN = serial_no(),
        Res = erlmachine_assembly:serial_no(Args, SN),
        {ok, erlmachine:document(Header, Res), State}
    catch E:R ->
            erlmachine:failure(E, R, State)
    end.

{ok, Serial} = erlmachine_serial:update(?MODULE),
SN = erlmachine_serial_no:serial_no(Serial),

{ok, Serial} = erlmachine_serial:update(?MODULE),
Rotate = erlmachine_serial_no:serial_no(Serial, SN),

-spec serial_no(Serial::serial()) -> serial_no().
serial_no(Serial) ->
    <<_:32, _:32, _:32, _:32>> = erlmachine:guid(Serial).

-spec serial_no(Serial::serial(), SN::serial_no()) -> serial_no().
serial_no(Serial, <<B1:32, B2:32, B3:32, B4:32>>) ->
    B5 = erlang:phash2({B1, Serial}, 4294967296),
    <<(B2 bxor B5):32, (B3 bxor B5):32, (B4 bxor B5):32, B5:32>>.


-spec tabname() -> atom().
tabname() ->
    ?MODULE.

-spec record_name() -> atom().
record_name() ->
    serial.

-spec serial_no() -> atom().
serial_no() ->
    serial_no.

-spec part_no() -> atom().
part_no() ->
    part_no.

-spec attributes() -> list(atom()).
attributes() ->
    record_info(fields, serial).

attributes(Attr) ->
    {attributes, Attr}.

record_name(Name) ->
    {record_name, Name}.

-spec update(Id::atom()) -> success(integer()).
update(Id) ->
    erlmachine:success(update(ID, 1)).

-spec update(Id::atom(), Value::integer()) -> success(integer()).
update(Id, Value) ->
    mnesia:dirty_update_counter(tabname(), Id, Value).

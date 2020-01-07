-module(erlmachine_warehouse).

-folder(<<"erlmachine/warehouse">>).

-steps([]).

-behaviour(gen_server).

%% API.
-export([start_link/0]).

-export([]).

%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-export([assembly/2, assembly/1]).

-include("erlmachine_factory.hrl").
-include("erlmachine_system.hrl").
-include("erlmachine_filesystem.hrl").

%% I guess warehouse is only responsible for storing and retrieving assembly accordingly to predefined capactiy;
%% The responsibility of assembly process needs to be passed to factory;

%% We need to provide broken part issue callback for reason which is oposite to normal;
%% Correct processing of issue will be provided by factory;
%% It's need to be sent to warehouse;
%% It's a not the same like standard unmount;
%% The specialized "crash" callback from factory will be provided;
%% From warehouse perspective are two key's message exist - produced and broken;
%% I am going to store rejected parts on warehouse;
%% We need to provide the specialized callback for this;

 
-record(state, { gearbox::assembly() }).

%% API.

id() -> 
    ?MODULE.

-spec start_link() -> success(pid()) | ingnore | failure(E::term()).
start_link() ->
    ID = id(),
    gen_server:start_link({local, ID}, ?MODULE, [], []).

-spec assembly(SN::serial_no()) -> 
                      success(assembly()) | failure(E::term(), R::term()).
assembly(SN) ->
    read(SN).

-spec assembly(SN::serial_no(), Assembly::assembly()) -> 
                      success().
assembly(SN, Assembly) ->
    update(SN, Assembly).

-record(read, { id::serial_no() }).

-spec read(ID::serial_no()) -> assembly().
read(ID) ->
    %% Just default timeout for the first time;
    gen_server:call(id(), #read{ id=ID }).

-record(update, { id::serial_no(), object::assembly() }).

-spec update(ID::serial_no(), Object::assembly()) -> success().
update(ID, Object) ->
    erlang:send(id(), #update{ id=ID, object=Object }).
 
%% gen_server.

init([]) ->
    GearBoxModel = gearbox_warehouse,
    GearBoxModelOpt = [],
    Env = [],
    GearBox = erlmachine_factory:gearbox(GearBoxModel, GearBoxModelOpt, Env),

    GearReplyModel = gear_reply,
    GearReply = erlmachine_factory:gear(GearBox, GearReplyModel, []),

    GearMnesiaModel = gear_mnesia,
    Name = assembly, Attributes = erlmachine_assembly:fields(), Nodes = [node()],
    GearMnesiaOpt = [
                     {name, Name}, 
                     {tabdef, [{attributes, Attributes}, {disc_copies, Nodes}, {record_name, Name}]}
                    ],
    GearMnesia = erlmachine_factory:gear(GearBox, GearMnesiaModel, GearMnesiaOpt),

    BuildGearMnesia = erlmachine_gear:parts(GearMnesia, [GearReply]),

    AxleModel = axle_tracker,
    Axle = erlmachine_factory:axle(GearBox, AxleModel, []),

    BuildAxle = erlmachine_axle:parts(Axle, [BuildGearMnesia]),

    Input = erlmachine_assembly:serial_no(GearMnesia),
    Parts = [
             GearReply,
             BuildAxle
            ],
    
    BuildGearBox = erlmachine_gearbox:input(erlmachine_gearbox:parts(GearBox, Parts), Input),

    {ok, _PID} = erlmachine_assembly:install(BuildGearBox),

    {ok, #state{ gearbox=BuildGearBox }}.

handle_call(#read{ id=ID }, _From, #state{ gearbox=GearBox } = State) ->
    Result = erlmachine_gearbox:transmit(GearBox, #{ read => ID }),
    
    {reply, Result, State};

handle_call(_Request, _From, State) ->
	{reply, ignored, State}.

handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info(#update{ id=ID, object=Object }, #state{ gearbox=GearBox } = State) ->
    io:format("~n~p~nUpdate: ~p~n~p~n",[?MODULE, ID, Object]),

    erlmachine_gearbox:rotate(GearBox, erlmachine:command(#{ write => Object })),
    {noreply, State};

handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

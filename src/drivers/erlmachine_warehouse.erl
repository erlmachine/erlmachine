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

-export([load/1]).
-export([unload/1]).

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

 
-record(state, {
}).

%% API.

id() -> 
    ?MODULE.

-spec start_link() -> success(pid()) | ingnore | failure(E::term()).
start_link() ->
    ID = id(),
    gen_server:start_link({local, ID}, ?MODULE, [], []).

-record(load, {assembly::assembly()}).

-spec load(Assembly::assembly()) -> ok.
load(Assembly) ->
    %% Just default timeout for the first time;
    ID = id(),
    erlang:send(ID, #load{assembly=Assembly}),
    ok.

-record(unload, {serial_no::serial_no()}).

-spec unload(SN::serial_no()) -> assembly().
unload(_SN) ->
    <<"">>.
 
%% gen_server.

init([]) ->
    GearBoxModel = gearbox_warehouse,
    GearBoxModelOpt = [],
    Env = [], %% That is suitable place for taple configuration;
    GearBox = erlmachine_factory:gearbox(GearBoxModel, GearBoxModelOpt, Env),

    GearTranslatorModel = gear_assembly_translator,
    GearTranslatorOpt = [],
    _GearTranslator = erlmachine_factory:gear(GearBox, GearTranslatorModel, GearTranslatorOpt),

    GearMnesiaModel = gear_mnesia,
    Name = assembly, Attributes = erlmachine_assembly:fields(), Nodes = [node()],
    GearMnesiaOpt = [{name, Name}, {tabdef, [{attributes, Attributes}, {disc_copies, Nodes}]}],
    GearMnesia = erlmachine_factory:gear(GearBox, GearMnesiaModel, GearMnesiaOpt),
    
    ShaftMnesiaModel = shaft_mnesia,
    ShaftMnesiaOpt = [],
    ShaftMnesia = erlmachine_factory:shaft(GearBox, ShaftMnesiaModel, ShaftMnesiaOpt),
    
    _BuildShaftMnesia = erlmachine_shaft:parts(ShaftMnesia, [GearMnesia]),

    AxleModel = axle_tracker,

    _AxleHttp = erlmachine_factory:axle(GearBox, AxleModel, []),
    
    {ok, #state{}}.

handle_call(#load{}, _From, State) ->
    {reply, ignored, State};

handle_call(#unload{}, _From, State) ->
    {reply, ignored, State};

handle_call(_Request, _From, State) ->
	{reply, ignored, State}.

handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

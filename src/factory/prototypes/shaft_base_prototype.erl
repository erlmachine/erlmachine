-module(shaft_base_prototype).
-behaviour(gen_server).

-folder(<<"erlmachine/factory/prototypes/shaft_base_prototype">>).

-behaviour(erlmachine_assembly).
-behaviour(erlmachine_transmission).
-behaviour(erlmachine_tracker).
-behaviour(erlmachine_system).
-behaviour(gen_server).

%% API.
-export([start_link/0]).

%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-include("erlmachine_factory.hrl").
-include("erlmachine_system.hrl").

%% API.

%% I guess factory will write to catalogue via catalogue behaviour;
%% The main purpose of prototype is to provide implemetation for communication and configuration;
%% The main purpose of detail is to provide mechanical control API around internal isolated structure;
%% The main purpose of the model is to provide mechanical reflection of modelling process over whole assembly;

-spec tag(Package::map()) -> Tag::binary().
tag(#{model := Model}) ->
    ID = atom_to_binary(Model, latin1),
    ID.

format_name(SerialNumber) ->
    ID = erlang:binary_to_atom(SerialNumber, latin1),
    ID.

-spec install(Name::serial_number(), Assembly::assembly(), Options::list()) -> success(pid()) | ingnore | failure(E).
install(Name, Assembly, Options) ->
    gen_server:start_link({local, format_name(SN)}, ?MODULE, Assembly, Options).


-record(attach, {part::assembly()}).%% Shift pattern

-spec attach(Name::serial_number(), Part::assembly(), Timeout::timeout()) -> success(Release::assembly()) | failure(E, R).
attach(Name, Assembly, Timeout) ->
    gen_server:call(format_name(Name), #attach{part = Assembly}, Timeout).

-record(detach, {id::serial_number()}).

-spec detach(Name::serial_number(), ID::serial_number(), Timeout::timeout()) -> success(Release::assembly()) | failure(E, R).
detach(Name, ID, TimeOut) ->
    gen_server:call(format_name(Name), #detach{id = ID}, TimeOut).

-record(overload, {load::load()}).

-spec overload(Name::serial_number(), Load::term()) -> Load::term().
overload(Name, Load) ->
    erlang:send(format_name(Name), #load{load = Load}).

-record(blockage, {part::assembly()}).

-spec blockage(Name::serial_number(), Part::assembly()) -> Part::assembly().
blockage() ->
    erlang:send(Name, #blockage{part = Part}).

-record(replace, {repair::assembly()}).

-spec replace(Name::serial_number(), Repair::assembly(), Timeout::timeout()) -> success(Release::assembly()) | failure(E, R).
replace(Name, Repair) ->
    gen_server:call(format_name(Name), #replace{repair = Repair}, TimeOut).

rotate(ID, Motion) ->
    erlang:send(ID, Force),
    Force.

transmit(ID, Motion) ->
    gen_server:call(ID, Force).

-spec uninstall(ID::atom(), Reason::term(), Timeout::timeout()) -> ok.
uninstall(ID, Reason, Timeout) ->
    gen_server:stop(ID, Reason, Timeout).

accept() -> %% TODO Acceptance criteria needs to be satisfied;
     ok.

%% gen_server.
-record(state, {tracking_number::tracking_number(), assembly::assembly()}).

%% From erlmachine_assembly we wait the specialized callback to erlmachine_gear with install/2
init(Assembly::term()) ->
    process_flag(trap_exit, true),
    Release = erlmachine_shaft:install(Assembly),
    %% I guess tracking time will be filled by tracker itself;
    Package = package(Release),
    TrackingNumber = erlmachine_traker:tracking_number(?MODULE, Package),
    erlmachine_traker:trace(TrackingNumber, #{init => Package}),
    {ok, #state{tracking_number = TrackingNumber, assembly = Release}}.

handle_call(#attach{part = Part}, _From, #state{} = State) ->
    #state{tracking_number = TrackingNumber, assembly = Assembly} = State,
    Release = erlmachine_shaft:attach(Assembly, Part),
    SerialNumber = erlmachine_assembly:serial_number(Part),
    Package = package(Release),
    erlmachine_traker:trace(TrackingNumber, #{attach => Package, part => SerialNumber}),
    {reply, {ok, Release}, State#state{assembly = Release}};

handle_call(#detach{id = ID}, _From,  #state{} = State) ->
    #state{tracking_number = TrackingNumber, assembly = Assembly} = State,
    Release = erlmachine_shaft:detach(Assembly, ID),
    Package = package(Release),
    erlmachine_traker:trace(TrackingNumber, #{detach => Package, part => ID}),
    {reply, {ok, Release}, State#state{assembly = Release}};

handle_call(#replace{repair = Repair}, _From, #state{} = State) ->
    #state{tracking_number = TrackingNumber, assembly = Assembly} = State,
    Release = erlmachine_shaft:replace(Assembly, Repair),
    SerialNumber = erlmachine_assembly:serial_number(Repair),
    Package = package(Release),
    erlmachine_traker:trace(TrackingNumber, #{replace => Package, repair => SerialNumber}),
    {reply, {ok, Release}, State#state{assembly = Release}};
    
    
    
    
    




handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info(_Info, State) ->
	{noreply, State}.

%% When reason is different from normal, or stop - the broken part event is occured;
terminate(Reason, Assembly) ->
    Package = package(Assembly),
    TrackingNumber = erlmachine_traker:tracking_number(?MODULE, Package),
    erlmachine_traker:trace(TrackingNumber, #{terminate => Package, reason => Reason}),
    (Reason == normal) orelse erlmachine_system:crash(Reason, Assembly),
    ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

package(Assembly) ->
    Model = erlmachine_assembly:model(Assembly),
    SerialNumber = erlmachine_assembly:serial_number(Assembly),
    Package = #{prototype => ?MODULE, model => Model, serial_number => SerialNumber},
    Package.

trace(Package) ->
    TrackingNumber = erlmachine_traker:tracking_number(?MODULE, Package),
    erlmachine_traker:trace(TrackingNumber, Package).

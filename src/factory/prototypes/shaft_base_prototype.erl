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
%% The main purpose of prototype is to provide implemetation of both communication and configuration layers;
%% The main purpose of detail is to provide mechanical intraface API over internal isolated product structure;
%% The main purpose of model is to provide mechanical reflection of modelling process over whole assembly;

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

-record(attach, {part::assembly()}).

-spec attach(Name::serial_number(), Part::assembly(), Timeout::timeout()) -> success(Release::assembly()) | failure(E, R).
attach(Name, Part, Timeout) ->
    gen_server:call(format_name(Name), #attach{part = Part}, Timeout).

-record(detach, {id::serial_number()}).

-spec detach(Name::serial_number(), ID::serial_number(), Timeout::timeout()) -> success(Release::assembly()) | failure(E, R).
detach(Name, ID, TimeOut) ->
    gen_server:call(format_name(Name), #detach{id = ID}, Timeout).

-record(overload, {load::load()}).

-spec overload(Name::serial_number(), Load::term()) -> Load.
overload(Name, Load) ->
    erlang:send(format_name(Name), #overload{load = Load}).

-record(blockage, {part::assembly()}).

-spec blockage(Name::serial_number(), Part::assembly(), Failure::failure(E, R)) -> Part.
blockage() ->
    erlang:send(format_name(Name), #blockage{part = Part, failure = Failure}).

-record(replace, {repair::assembly()}).

-spec replace(Name::serial_number(), Repair::assembly(), Timeout::timeout()) -> success(Release::assembly()) | failure(E, R).
replace(Name, Repair) ->
    gen_server:call(format_name(Name), #replace{repair = Repair}, Timeout).

-record(rotate, {motion::term()}).

-spec rotate(Name::serial_number(), Motion::term()) -> Motion.
rotate(Name, Motion) ->
    erlang:send(format_name(Name), #rotate{motion = Motion}).

-record(transmit, {motion::term()}).

-spec transmit(Name::serial_number(), Motion::term(), Timeout::timeout()) -> Force::term().
transmit(Name, Motion) ->
    gen_server:call(ID, #transmit{motion = Motion}, Timeout).

-spec uninstall(Name::serial_number(), Reason::term(), Timeout::timeout()) -> ok.
uninstall(ID, Reason, Timeout) ->
    gen_server:stop(ID, Reason, Timeout).

-record(accept, {criteria::acceptance_criteria()}).

-spec accept(Name::serial_number(), Criteria::acceptance_criteria()) -> accept() | reject().
accept(Name, Criteria) -> %% I plan to reflect criteria in datasheet; 
    gen_server:call(Name, #accept{criteria = Criteria}, Timeout).

%% gen_server.
-record(state, {tracking_number::tracking_number(), assembly::assembly()}).

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
    SerialNumber = erlmachine_factory:serial_number(Part),
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
    SerialNumber = erlmachine_factory:serial_number(Repair),
    Package = package(Release),
    erlmachine_traker:trace(TrackingNumber, #{replace => Package, repair => SerialNumber}),
    {reply, {ok, Release}, State#state{assembly = Release}};

handle_call(#transmit{motion = Motion}, _From, #state{} = State) ->
    #state{assembly = Assembly} = State,
    %% Transmit is a direct call to the current mechanical part;
    %% It's always synchronous call;
    %% This point is a place denoted to the  management API of the current mechanical instance;
    Force = erlmachine_shaft:transmit(Assembly, Motion),
    {reply, Force, State};

handle_call(#accept{criteria = Criteria}, _From, #state{} = State) ->
    #state{tracking_number = TrackingNumber, assembly = Assembly} = State,
    Result = erlmachine_factory:accept(Assembly, Criteria),
    SerialNumber = erlmachine_factory:serial_number(Repair),
    Package = package(Assembly),
    if Result == true ->
            erlmachine_traker:trace(TrackingNumber, #{accept => Package});
       true -> 
            erlmachine_traker:trace(TrackingNumber, #{reject => Package, report => Result}),
            erlmachine_system:reject(Reason, Assembly)
    end,
    {reply, Result, State};

handle_call(Req, _From, #state{} = State) ->
    #state{tracking_number = TrackingNumber, assembly = Assembly} = State,
    SerialNumber = erlmachine_factory:serial_number(Assembly),
    Package = package(Assembly),
    erlmachine_traker:trace(TrackingNumber, #{req => Req}),
    {reply, ignored, State}.

handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info(#rotate{motion = Motion}, State) ->
    #state{assembly = Assembly} = State,
    erlmachine_shaft:rotate(Assembly, Motion), 
    %% At the moment we support only default rotation (all parts will be rotated without certain args);
    %% That works very similar to direct exchange;
    %% We going to provide controlled rotation from our next versions (serial number will be passed as argument);
    %% Serial numbers of parts will be stored inside prototype. It allows to support different shift patterns;
    {noreply, State};

handle_info(#overload{load = Load}) ->
    #state{tracking_number = TrackingNumber, assembly = Assembly} = State,
    Package = package(Assembly),
    erlmachine_traker:trace(TrackingNumber, #{overload => Package, load => Load}),
    {noreply, State};

handle_info(#blockage{part = Part, damage = Damage}, State) ->
    #state{tracking_number = TrackingNumber, assembly = Assembly} = State,
    Package = package(Release),
    erlmachine_traker:trace(TrackingNumber, #{blockage => Package, part => Part, damage => Damage}),
    erlmachine_system:damage(Assembly, Damage),
    {noreply, State}.

%% When reason is different from normal, or stop - the broken part event is occured;
terminate(Reason, Assembly) ->
    erlmachine_shaft:uninstall(Assembly),
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

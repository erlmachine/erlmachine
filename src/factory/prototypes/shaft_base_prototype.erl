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

-record(install, {gearbox::gearbox(), assembly::assembly()}).

-spec install(Name::serial_number(), GearBox::assembly(), Assembly::assembly()) -> success(pid()) | ingnore | failure(E).
install(Name, GearBox, Assembly) ->
    gen_server:start_link({local, format_name(SN)}, ?MODULE, #install{gearbox=GearBox, assembly=Assembly}, []).

-record(attach, {gearbox::assembly(), part::assembly()}).

-spec attach(Name::serial_number(), GearBox::assembly(), Part::assembly(), Timeout::timeout()) -> success(Release::assembly()) | failure(E, R).
attach(Name, GearBox, Part, Timeout) ->
    gen_server:call(format_name(Name), #attach{gearbox=GearBox, part=Part}, Timeout).

-record(detach, {assembly::assembly(), id::serial_number()}).

-spec detach(Name::serial_number(), GearBox::assembly(), ID::serial_number(), Timeout::timeout()) -> success(Release::assembly()) | failure(E, R).
detach(Name, GearBox, ID, TimeOut) ->
    gen_server:call(format_name(Name), #detach{assembly=GearBox, id=ID}, Timeout).

-record(overload, {assembly::assembly(), load::load()}).

-spec overload(Name::serial_number(), Assembly::assembly(), Load::term()) -> Load.
overload(Name, Assembly, Load) ->
    erlang:send(format_name(Name), #overload{assembly=Assembly,load=Load}).

-record(block, {assembly::assembly(), part::assembly()}).

-spec block(Name::serial_number(), Assembly::assembly(), Part::assembly(), Failure::failure(E, R)) -> Part.
block(Name, Assembly, Part) ->
    erlang:send(format_name(Name), #block{assembly=Assembly, part=Part, failure=Failure}).

-record(replace, {assembly::assembly(), repair::assembly()}).

-spec replace(Name::serial_number(), Assembly::assembly(), Repair::assembly(), Timeout::timeout()) -> success(Release::assembly()) | failure(E, R).
replace(Name, Assembly, Repair) ->
    gen_server:call(format_name(Name), #replace{assembly=Assembly, repair=Repair}, Timeout).

-record(rotate, {assembly::assembly(), motion::term()}).

-spec rotate(Name::serial_number(), Assembly::assembly(), Motion::term()) -> Motion.
rotate(Name, Assembly, Motion) ->
    erlang:send(format_name(Name), #rotate{assembly=Assembly, motion=Motion}).

-record(transmit, {assembly::assembly(), motion::term()}).

-spec transmit(Name::serial_number(), Assembly::assembly(),  Motion::term(), Timeout::timeout()) -> Force::term().
transmit(Name, Assembly, Motion) ->
    gen_server:call(ID, #transmit{assembly=Assembly, motion=Motion}, Timeout).

-spec uninstall(Name::serial_number(), Assembly::assembly(), Reason::term(), Timeout::timeout()) -> ok.
uninstall(ID, Assembly, Reason, Timeout) ->
    gen_server:stop(ID, Reason, Timeout).

-record(accept, {assembly::assembly(), criteria::acceptance_criteria()}).

-spec accept(Name::serial_number(), Assembly::assembly(), Criteria::acceptance_criteria()) -> accept() | reject().
accept(Name, Criteria) -> %% I plan to reflect criteria in datasheet; 
    gen_server:call(Name, #accept{assembly=Assembly, criteria=Criteria}, Timeout).

%% gen_server.
-record(state, {assembly::assembly(), tracking_number::tracking_number()}).

init(#install{gearbox=GearBox, assembly=Assembly}) ->
    process_flag(trap_exit, true),
    {ok, Release} = erlmachine_shaft:install(GearBox, Assembly),
    %% I guess tracking time will be filled by tracker itself;
    Package = package(Release),
    TrackingNumber = erlmachine_traker:tracking_number(?MODULE, Package),
    erlmachine_traker:trace(TrackingNumber, #{init => Package}),
    {ok, #state{tracking_number=TrackingNumber, assembly=Release}}.

handle_call(#attach{gearbox=GearBox, part=Part}, _From, #state{} = State) ->
    #state{tracking_number=TrackingNumber} = State,
    Result = erlmachine_shaft:attach(Gearbox, Part),
    {reply, Result, State};

handle_call(#detach{assembly=Assembly, id=ID}, _From, #state{} = State) ->
    #state{tracking_number = TrackingNumber} = State,
    Result = erlmachine_shaft:detach(Assembly, ID),
    {reply, Result, State};

handle_call(#replace{assembly=Assembly, repair=Repair}, _From, #state{} = State) ->
    #state{tracking_number = TrackingNumber} = State,
    Result = erlmachine_shaft:replace(Assembly, Repair),
    {reply, Result, State};

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
    {noreply, State};

handle_info(#block{part = Part, damage = Damage}, State) ->
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

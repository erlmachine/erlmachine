-module(shaft_base_prototype).

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

format_name(SerialNumber) ->
    ID = erlang:binary_to_atom(SerialNumber, latin1),
    ID.

-record(install, {gearbox::gearbox(), shaft::assembly()}).

-spec install(Name::serial_number(), GearBox::assembly(), Shaft::assembly()) -> 
                     success(pid()) | ingnore | failure(E).
install(Name, GearBox, Shaft) ->
    gen_server:start_link({local, format_name(Name)}, ?MODULE, #install{gearbox=GearBox, shaft=Shaft}, []).

-record(attach, {part::assembly()}).

-spec attach(Name::serial_number(), Part::assembly(), Timeout::timeout()) -> 
                    success(Release::assembly()) | failure(E, R).
attach(Name, Part, Timeout) ->
    gen_server:call(format_name(Name), #attach{part=Part}, Timeout).

-record(detach, {id::serial_number()}).

-spec detach(Name::serial_number(), ID::serial_number(), Timeout::timeout()) ->
                    success(Release::assembly()) | failure(E, R).
detach(Name, ID, TimeOut) ->
    gen_server:call(format_name(Name), #detach{id=ID}, Timeout).

-record(overload, {load::load()}).

-spec overload(Name::serial_number(), Load::term()) ->
                      Load.
overload(Name, Load) ->
    erlang:send(format_name(Name), #overload{load=Load}).

-record(block, {part::assembly(), failure::failure()}).

-spec block(Name::serial_number(), Failure::failure(E, R)) -> 
                   Part.
block(Name, Part, Failure) ->
    erlang:send(format_name(Name), #block{part=Part, failure=Failure}).

-record(replace, {repair::assembly()}).

-spec replace(Name::serial_number(), Repair::assembly(), Timeout::timeout()) -> 
                     success(Release::assembly()) | failure(E, R).
replace(Name, Repair) ->
    gen_server:call(format_name(Name), #replace{repair=Repair}, Timeout).

-record(rotate, {motion::term()}).

-spec rotate(Name::serial_number(), Motion::term()) -> 
                    Motion.
rotate(Name, Motion) ->
    erlang:send(format_name(Name), #rotate{motion=Motion}).

-record(transmit, {motion::term()}).

-spec transmit(Name::serial_number(), Motion::term(), Timeout::timeout()) ->
                      Force::term().
transmit(Name, Motion) ->
    gen_server:call(ID, #transmit{motion=Motion}, Timeout).

-spec uninstall(Name::serial_number(), Reason::term(), Timeout::timeout()) ->
                       ok.
uninstall(ID, Reason, Timeout) ->
    gen_server:stop(ID, Reason, Timeout).

-record(accept, {criteria::acceptance_criteria()}).

-spec accept(Name::serial_number(), Criteria::acceptance_criteria()) ->
                    accept() | reject().
accept(Name, Criteria) -> 
    gen_server:call(Name, #accept{criteria=Criteria}, Timeout).

%% gen_server.
-record(state, {gearbox::assembly(), shaft::assembly()}).

init(#install{gearbox=GearBox, shaft=Shaft}) ->
    process_flag(trap_exit, true),
    {ok, Release} = erlmachine_shaft:install(GearBox, Shaft),
    {ok, #state{gearbox=Gearbox, shaft=Release}}.

handle_call(#attach{part=Part}, _From, #state{gearbox=GearBox, shaft=Shaft} = State) ->
    Result = {ok, Release} = erlmachine_shaft:attach(Gearbox, Shaft, Part),
    {reply, Result, State#state{shaft=Release}};

handle_call(#detach{id=ID}, _From, #state{gearbox=Gearbox, shaft=Shaft} = State) ->
    Result = {ok, Release} = erlmachine_shaft:detach(Gearbox, Shaft, ID),
    {reply, Result, State#state{shaft=Release}};

handle_call(#replace{repair=Repair}, _From, #state{gearbox=GearBox, shaft=Shaft} = State) ->
    Result = {ok, Release} = erlmachine_shaft:replace(Gearbox, Shaft, Repair),
    {reply, Result, State#state{shaft=Release}};

handle_call(#transmit{motion = Motion}, _From, #state{gearbox=GearBox, shaft=Shaft} = State) ->
    %% Transmit is a direct call to the current mechanical part;
    %% It's always synchronous call;
    %% This point is a place denoted to the  management API of the current mechanical instance;
    {ok, Result, Release} = erlmachine_shaft:transmit(Gearbox, Shaft, Motion),
    {reply, Result, State#state{shaft=Release}};

handle_call(#accept{criteria = Criteria}, _From, #state{gearbox=GearBox, shaft=Shaft} = State) ->
    Result = {ok, Release} = erlmachine_shaft:accept(Gearbox, Shaft, Criteria),
    {reply, Result, State#state{shaft=Release}};

handle_call(Req, _From,  #state{gearbox=Gearbox, shaft=Shaft}=State) ->
    erlmachine_shaft:call(Gearbox, Shaft, Req),
    {reply, ignored, State}.

handle_cast(Message, #state{gearbox=Gearbox, shaft=Shaft}=State) ->
    erlmachine_shaft:cast(Gearbox, Shaft, Message),
    {noreply, State}.

handle_info(#rotate{motion = Motion}, #state{gearbox=Gearbox, shaft=Shaft}=State) ->
    {ok, Release} = erlmachine_shaft:rotate(GearBox, Shaft, Motion),
    %% At the moment we support only default rotation (all parts will be rotated without certain args);
    %% That works very similar to direct exchange;
    %% We going to provide controlled rotation from our next versions (serial number will be passed as argument);
    %% Serial numbers of parts will be stored inside prototype. It allows to support different shift patterns;
    {noreply, State#state{shaft=Release}};

handle_info(#overload{load = Load}, #state{gearbox=Gearbox, shaft=Shaft}=State) ->
    {ok, Release} = erlmachine_shaft:overload(Gearbox, Shaft, Load),
    {noreply, State#state{shaft=Release}};

handle_info(#block{part=Part, failure = Failure}, #state{gearbox=Gearbox, shaft=Shaft}=State) ->
    {ok, Release} = erlmachine_shaft:block(Gearbox, Shaft, Part, Failure),
    {noreply, State#state{shaft=Release}};

handle_info(Message, #state{gearbox=Gearbox, shaft=Shaft}=State) ->
    erlmachine_shaft:info(Gearbox, Shaft, Message),
    {noreply, State}.

%% When reason is different from normal, or stop - the broken part event is occured;
terminate(Reason, #state{gearbox=Gearbox, shaft=Shaft}=State) ->
    erlmachine_shaft:uninstall(Gearbox, Shaft, Reason),
    ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

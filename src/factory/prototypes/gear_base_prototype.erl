-module(gear_base_prototype).

-folder(<<"erlmachine/factory/prototypes/gear_base_prototype">>).

%%-behaviour(erlmachine_assembly).
%%-behaviour(erlmachine_transmission).
-behaviour(erlmachine_tracker).
%%-behaviour(erlmachine_system).
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

format_name(SerialNumber) ->
    ID = erlang:binary_to_atom(SerialNumber, latin1),
    ID.

-record(install, {gearbox::gearbox(), gear::assembly(), options::list(tuple())}).

-spec install(Name::serial_number(), GearBox::assembly(), Gear::assembly(), Options::list(tuple())) -> 
                     success(pid()) | ingnore | failure(E).
install(Name, GearBox, Gear, Options) ->
    ID = {local, format_name(Name)},
    gen_server:start_link(ID, ?MODULE, #install{gearbox=GearBox, gear=Gear, options=Options}, []).

%% I think about ability to reflect both kind of switching - manual and automated;
-record(switch, {part::assembly()}).

-spec switch(Name::serial_number(), Part::assembly(), Timeout::timeout()) -> 
                    success(Release::assembly()) | failure(E, R).
switch(Name, Part, Timeout) ->
    gen_server:call(format_name(Name), #switch{part = Part}, Timeout).

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
uninstall(Name, Reason, Timeout) ->
    gen_server:stop({local, format_name(Name)}, Reason, Timeout).

-record(accept, {criteria::acceptance_criteria()}).

-spec accept(Name::serial_number(), Criteria::acceptance_criteria(), Timeout::timeout()) ->
                    accept() | reject().
accept(Name, Criteria, Timeout) -> 
    gen_server:call(Name, #accept{criteria=Criteria}, Timeout).

%% gen_server.
-record(state, {gearbox::assembly(), gear::assembly()}).

init(#install{gearbox=GearBox, gear=Gear, options=Options}) ->
    [process_flag(ID, Param)|| {ID, Param} <- Options],
    %% process_flag(trap_exit, true), Needs to be passed by default;
    %% Gearbox is intended to use like specification of destination point (it's not about persistence);
    {ok, Release} = erlmachine_gear:install(GearBox, Gear),
    {ok, #state{gearbox=Gearbox, gear=Release}}.

handle_call(#switch{part = Part}, _From, #state{gearbox=Gearbox, gear=Gear} = State) ->
    Result = {ok, Release} = erlmachine_gear:switch(Gearbox, Gear, Part),
    {reply, Result, State#state{gear=Release}};

handle_call(#replace{repair=Repair}, _From, #state{gearbox=GearBox, gear=Gear} = State) ->
    Result = {ok, Release} = erlmachine_gear:replace(Gearbox, Gear, Repair),
    {reply, Result, State#state{gear=Release}};

handle_call(#transmit{motion = Motion}, _From, #state{gearbox=GearBox, gear=Gear} = State) ->
    {ok, Result, Release} = erlmachine_gear:transmit(Gearbox, Gear, Motion),
    {reply, Result, State#state{gear=Release}};

handle_call(#accept{criteria = Criteria}, _From, #state{gearbox=GearBox, gear=Gear} = State) ->
    Result = {ok, Release} = erlmachine_gear:accept(Gearbox, Gear, Criteria),
    {reply, Result, State#state{gear=Release}};

handle_call(Req, _From,  #state{gearbox=Gearbox, gear=Gear}=State) ->
    erlmachine_gear:call(Gearbox, Gear, Req),
    {reply, ignored, State}.

handle_cast(Message, #state{gearbox=Gearbox, gear=Gear}=State) ->
    erlmachine_gear:cast(Gearbox, Gear, Message),
    {noreply, State}.

handle_info(#rotate{motion = Motion}, #state{gearbox=Gearbox, gear=Gear}=State) ->
    {ok, Release} = erlmachine_gear:rotate(GearBox, Gear, Motion),
    %% Potentially clients can provide sync delivery inside this call;
    %% It can work a very similar to job queue);
    {noreply, State#state{gear=Release}};

handle_info(#overload{load = Load}, #state{gearbox=Gearbox, gear=Gear}=State) ->
    {ok, Release} = erlmachine_gear:overload(Gearbox, Gear, Load),
    {noreply, State#state{gear=Release}};

handle_info(#block{part=Part, failure = Failure}, #state{gearbox=Gearbox, gear=Gear}=State) ->
    %% Damage, Crash and Failure will be translated to specialized system gears;
    %% This produced stream can be consumed by custom components which can be able to provide repair;
    {ok, Release} = erlmachine_gear:block(Gearbox, Gear, Part, Failure),
    {noreply, State#state{gear=Release}};

handle_info(Message, #state{gearbox=Gearbox, gear=Gear}=State) ->
    erlmachine_gear:info(Gearbox, Gear, Message),
    {noreply, State}.

%% When reason is different from normal, or stop - the broken part event is occured;
terminate(Reason, #state{gearbox=Gearbox, gear=Gear}=State) ->
    erlmachine_gear:uninstall(Gearbox, Gear, Reason),
    ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

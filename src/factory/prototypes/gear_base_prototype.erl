-module(gear_base_prototype).

-folder(<<"erlmachine/factory/prototypes/gear_base_prototype">>).

-behaviour(gen_server).

%% API.

-export([name/0]).

%% gen_server.
-export([
         init/1, 
         handle_call/3, handle_cast/2, handle_info/2, 
         terminate/2, 
         code_change/3
        ]).

-export([
         install/4, 
         switch/3, 
         overload/2, block/3, 
         replace/3,
         rotate/2, transmit/3, %% transmit/4
         uninstall/3,
         accept/3
        ]).

-include("erlmachine_factory.hrl").
-include("erlmachine_system.hrl").

%% API.

-spec name() -> atom().
name() ->
    ?MODULE.

format_name(SerialNumber) ->
    ID = erlang:binary_to_atom(SerialNumber, latin1),
    ID.

-record(install, {gearbox::assembly(), gear::assembly(), options::list(tuple())}).

-spec install(Name::serial_no(), GearBox::assembly(), Gear::assembly(), Options::list(tuple())) -> 
                     success(pid()) | ingnore | failure(E::term()).
install(Name, GearBox, Gear, Options) ->
    ID = {local, format_name(Name)},
    gen_server:start_link(ID, ?MODULE, #install{gearbox=GearBox, gear=Gear, options=Options}, []).

%% I think about ability to reflect both kind of switching - manual and automated;
-record(switch, {part::assembly()}).

-spec switch(Name::serial_no(), Part::assembly(), Timeout::timeout()) -> 
                    success(Release::assembly()) | failure(E::term(), R::term()).
switch(Name, Part, Timeout) ->
    gen_server:call(format_name(Name), #switch{part = Part}, Timeout).

-record(overload, {load::term()}).

-spec overload(Name::serial_no(), Load::term()) ->
                      Load::term().
overload(Name, Load) ->
    erlang:send(format_name(Name), #overload{load=Load}), 
    Load.

-record(block, {part::assembly(), failure::term()}).

-spec block(Name::serial_no(), Part::assembly(), Failure::term()) -> 
                   Failure::term().
block(Name, Part, Failure) ->
    erlang:send(format_name(Name), #block{part=Part, failure=Failure}), 
    Failure.

-record(replace, {repair::assembly()}).

-spec replace(Name::serial_no(), Repair::assembly(), Timeout::timeout()) -> 
                     success(Release::assembly()) | failure(E::term(), R::term()).
replace(Name, Repair, Timeout) ->
    gen_server:call(format_name(Name), #replace{repair=Repair}, Timeout).

-record(rotate, {motion::term()}).

-spec rotate(Name::serial_no(), Motion::term()) -> 
                    Motion::term().
rotate(Name, Motion) ->
    erlang:send(format_name(Name), #rotate{motion=Motion}), 
    Motion.

-record(transmit, {motion::term()}).

-spec transmit(Name::serial_no(), Motion::term(), Timeout::timeout()) ->
                      Force::term().
transmit(Name, Motion, Timeout) ->
    gen_server:call(format_name(Name), #transmit{motion=Motion}, Timeout).

-spec uninstall(Name::serial_no(), Reason::term(), Timeout::timeout()) ->
                       ok.
uninstall(Name, Reason, Timeout) ->
    gen_server:stop({local, format_name(Name)}, Reason, Timeout).

-record(accept, {criteria::acceptance_criteria()}).

-spec accept(Name::serial_no(), Criteria::acceptance_criteria(), Timeout::timeout()) ->
                    accept() | reject().
accept(Name, Criteria, Timeout) -> 
    gen_server:call(Name, #accept{criteria=Criteria}, Timeout).

%% gen_server.
-record(state, {gearbox::assembly(), gear::assembly()}).

init(#install{gearbox=GearBox, gear=Gear, options=Options}) ->
    [process_flag(ID, Param)|| {ID, Param} <- Options],
    %% process_flag(trap_exit, true), Needs to be passed by default;
    %% Gearbox is intended to use like specification of destination point (it's not about persistence);
    {ok, Release} = erlmachine_gear:install_model(GearBox, Gear),
    {ok, #state{gearbox=GearBox, gear=Release}}.

handle_call(#switch{part = Part}, _From, #state{gearbox=Gearbox, gear=Gear} = State) ->
    Result = {ok, Release} = erlmachine_gear:switch_model(Gearbox, Gear, Part),
    {reply, Result, State#state{gear=Release}};

handle_call(#replace{repair=Repair}, _From, #state{gearbox=GearBox, gear=Gear} = State) ->
    Result = {ok, Release} = erlmachine_gear:replace_model(GearBox, Gear, Repair),
    {reply, Result, State#state{gear=Release}};

handle_call(#transmit{motion = Motion}, _From, #state{gearbox=GearBox, gear=Gear} = State) ->
    {ok, Result, Release} = erlmachine_gear:transmit_model(GearBox, Gear, Gear, Motion),
    {reply, Result, State#state{gear=Release}};

handle_call(#accept{criteria = Criteria}, _From, #state{gearbox=GearBox, gear=Gear} = State) ->
    {ok, Report, Release} = erlmachine_gear:accept_model(GearBox, Gear, Criteria),
    {reply, Report, State#state{gear=Release}};

handle_call(Req, _From,  #state{gearbox=GearBox, gear=Gear}=State) ->
    erlmachine_gear:call(GearBox, Gear, Req),
    {reply, ignored, State}.

handle_cast(Message, #state{gearbox=GearBox, gear=Gear}=State) ->
    erlmachine_gear:cast(GearBox, Gear, Message),
    {noreply, State}.

handle_info(#rotate{motion = Motion}, #state{gearbox=GearBox, gear=Gear}=State) ->
    Parts = erlmachine_gear:parts(Gear),
    {ok, Release} = erlmachine_shaft:rotate_model(GearBox, Gear, Motion),
    [erlmachine_shaft:rotate(GearBox, Part, Motion)|| Part <- Parts],
    %% Potentially clients can provide sync delivery inside this call;
    %% It can work a very similar to job queue);
    {noreply, State#state{gear=Release}};

handle_info(#overload{load = Load}, #state{gearbox=GearBox, gear=Gear}=State) ->
    {ok, Release} = erlmachine_gear:overload_model(GearBox, Gear, Load),
    {noreply, State#state{gear=Release}};

handle_info(#block{part=Part, failure = Failure}, #state{gearbox=GearBox, gear=Gear}=State) ->
    %% Damage, Crash and Failure will be translated to specialized system gears;
    %% This produced stream can be consumed by custom components which can be able to provide repair;
    {ok, Release} = erlmachine_gear:block_model(GearBox, Gear, Part, Failure),
    {noreply, State#state{gear=Release}};

handle_info(Message, #state{gearbox=GearBox, gear=Gear}=State) ->
    erlmachine_gear:info(GearBox, Gear, Message),
    {noreply, State}.

%% When reason is different from normal, or stop - the broken part event is occured;
terminate(Reason, #state{gearbox=GearBox, gear=Gear}) ->
    erlmachine_gear:uninstall_model(GearBox, Gear, Reason),
    ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

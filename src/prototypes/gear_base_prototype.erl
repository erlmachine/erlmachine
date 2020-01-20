-module(gear_base_prototype).

-folder(<<"erlmachine/prototypes/gear_base_prototype">>).

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
         attach/5, detach/4,
         overload/4, block/5, 
         replace/4,
         rotate/4, transmit/4,
         uninstall/4,
         accept/4
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
install(Name, GearBox, Gear, Opt) ->
    ID = {local, format_name(Name)},
    Command = #install{ gearbox=GearBox, gear=Gear, options=Opt },

    gen_server:start_link(ID, ?MODULE, Command, []).

-record(attach, {part::assembly(), register::term()}).

-spec attach(Name::serial_no(), GearBox::assembly(), Gear::assembly(), Register::term(), Part::assembly()) -> 
                    success(Release::assembly()) | failure(E::term(), R::term()).
attach(Name, _GearBox, _Gear, Register, Part) ->
    Command = #attach{ part=Part, register=Register },

    gen_server:call(format_name(Name), Command).

-record(detach, {id::serial_no()}).

-spec detach(Name::serial_no(), GearBox::assembly(), Gear::assembly(), ID::serial_no()) -> 
                    success(Release::assembly()) | failure(E::term(), R::term()).
detach(Name, _GearBox, _Gear, ID) ->
    Command = #detach{ id=ID },

    gen_server:call(format_name(Name), Command).

-record(overload, {load::term()}).

-spec overload(Name::serial_no(), GearBox::assembly(), Gear::assembly(), Load::term()) ->
                      Load::term().
overload(Name, _GearBox, _Gear, Load) ->
    Command = #overload{ load=Load },

    erlang:send(format_name(Name), Command), 
    Load.

-record(block, {part::assembly(), failure::term()}).

-spec block(Name::serial_no(), GearBox::assembly(), Gear::assembly(), Part::assembly(), Failure::term()) -> 
                   Failure::term().
block(Name, _GearBox, _Gear, Part, Failure) ->
    Command = #block{ part=Part, failure=Failure },

    erlang:send(format_name(Name), Command), 
    Failure.

-record(replace, {repair::assembly()}).

-spec replace(Name::serial_no(), GearBox::assembly(), Gear::assembly(), Repair::assembly()) -> 
                     success(Release::assembly()) | failure(E::term(), R::term()).
replace(Name, _GearBox, _Gear, Repair) ->
    Command = #replace{ repair=Repair },

    gen_server:call(format_name(Name), Command).

-record(rotate, {motion::term()}).

-spec rotate(Name::serial_no(), GearBox::assembly(), Gear::assembly(), Motion::term()) -> 
                    Motion::term().
rotate(Name, _GearBox, _Gear, Motion) ->
    Command = #rotate{ motion=Motion },

    erlang:send(format_name(Name), Command), 
    Motion.

-record(transmit, {motion::term()}).

-spec transmit(Name::serial_no(), GearBox::assembly(), Gear::assembly(), Motion::term()) ->
                      Force::term().
transmit(Name, _GearBox, _Gear, Motion) ->
    Command = #transmit{ motion=Motion },

    gen_server:call(format_name(Name), Command).

-record(accept, {criteria::acceptance_criteria()}).

-spec accept(Name::serial_no(), GearBox::assembly(), Gear::assembly(), Criteria::criteria()) ->
                    success() | failure(E::term(), R::term(), S::term()).
accept(Name, _GearBox, _Gear, Criteria) -> 
    Command = #accept{ criteria=Criteria },

    gen_server:call(format_name(Name), Command).

-spec uninstall(Name::serial_no(), GearBox::assembly(), Gear::assembly(), Reason::term()) ->
                       ok.
uninstall(Name, _GearBox, _Gear, Reason) ->
    gen_server:stop({local, format_name(Name)}, Reason).

%% gen_server.
-record(state, {gearbox::assembly(), gear::assembly()}).

init(#install{gearbox=GearBox, gear=Gear, options=Opt}) ->
    Flags = proplists:get_value(process_flags, Opt, []),
    [process_flag(ID, Param)|| {ID, Param} <- [{trap_exit, true}|Flags]],
    %% process_flag(trap_exit, true), Needs to be passed by default;
    %% Gearbox is intended to use like specification of destination point (it's not about persistence);
    {ok, Release} = erlmachine_gear:install(GearBox, Gear),
    {ok, #state{gearbox=GearBox, gear=Release}}.

handle_call(#attach{part = Part, register = Register}, _From, #state{gearbox=GearBox, gear=Gear} = State) ->
    Result = {ok, _, Release} = erlmachine_gear:attach(GearBox, Gear, Register, Part),
    {reply, Result, State#state{gear=Release}};

handle_call(#detach{id = ID}, _From, #state{gearbox=GearBox, gear=Gear} = State) ->
    Result = {ok, Release} = erlmachine_gear:detach(GearBox, Gear, ID),
    {reply, Result, State#state{gear=Release}};

handle_call(#replace{repair=Repair}, _From, #state{gearbox=GearBox, gear=Gear} = State) ->
    Result = {ok, Release} = erlmachine_gear:replace(GearBox, Gear, Repair),
    {reply, Result, State#state{gear=Release}};

handle_call(#transmit{motion = Motion}, _From, #state{gearbox=GearBox, gear=Gear} = State) ->
    {ok, Result, Release} = erlmachine_gear:transmit(GearBox, Gear, Motion),
    {reply, Result, State#state{gear=Release}};

handle_call(#accept{criteria = Criteria}, _From, #state{gearbox=GearBox, gear=Gear} = State) ->
    {ok, Status, _} = erlmachine_gear:accept(GearBox, Gear, Criteria),
    {reply, Status, State};

handle_call(Req, _From,  #state{gearbox=GearBox, gear=Gear}=State) ->
    erlmachine_gear:call(GearBox, Gear, Req),
    {reply, ignored, State}.

handle_cast(Message, #state{gearbox=GearBox, gear=Gear}=State) ->
    erlmachine_gear:cast(GearBox, Gear, Message),
    {noreply, State}.

handle_info(#rotate{motion = Motion}, #state{gearbox=GearBox, gear=Gear}=State) ->
    {ok, Release} = erlmachine_gear:rotate(GearBox, Gear, Motion),
    %% Potentially clients can provide sync delivery inside this call;
    %% It can work a very similar to job queue);
    {noreply, State#state{gear=Release}};

handle_info(#overload{load = Load}, #state{gearbox=GearBox, gear=Gear}=State) ->
    {ok, Release} = erlmachine_gear:overload(GearBox, Gear, Load),
    {noreply, State#state{gear=Release}};

handle_info(#block{part=Part, failure = Failure}, #state{gearbox=GearBox, gear=Gear}=State) ->
    %% Damage, Crash and Failure will be translated to specialized system gears;
    %% This produced stream can be consumed by custom components which can be able to provide repair;
    {ok, Release} = erlmachine_gear:block(GearBox, Gear, Part, Failure),
    {noreply, State#state{gear=Release}};

handle_info(Load, #state{gearbox=GearBox, gear=Gear}=State) ->
    %% At that place load capacity control can be achived;
    {ok, Release} = erlmachine_gear:load(GearBox, Gear, Load),
    {noreply, State#state{gear=Release}}.

%% When reason is different from normal, or stop - the broken part event is occured;
terminate(Reason, #state{gearbox=GearBox, gear=Gear}) ->
    erlmachine_gear:uninstall(GearBox, Gear, Reason),
    ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

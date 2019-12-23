-module(gear_syn_prototype).

-folder(<<"erlmachine/factory/prototypes/gear_syn_prototype">>).

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

format_name(Name) ->
    {via, syn, Name}.

-record(install, {gearbox::assembly(), gear::assembly(), options::list(tuple())}).

-spec install(Name::serial_no(), GearBox::assembly(), Gear::assembly(), Opt::list(tuple())) -> 
                     success(pid()) | ingnore | failure(E::term()).
install(Name, GearBox, Gear, Opt) ->
    gen_server:start_link(format_name(Name), ?MODULE, #install{gearbox=GearBox, gear=Gear, options=Opt}, []).

-record(attach, {part::assembly(), register::term()}).

-spec attach(Name::serial_no(), GearBox::assembly(), Gear::assembly(), Register::term(), Part::assembly()) -> 
                    success(Release::assembly()) | failure(E::term(), R::term()).
attach(Name, _GearBox, _Gear, Register, Part) ->
    gen_server:call(format_name(Name), #attach{part=Part, register=Register}).

-record(detach, {id::serial_no()}).

-spec detach(Name::serial_no(), GearBox::assembly(), Gear::assembly(), ID::serial_no()) -> 
                    success(Release::assembly()) | failure(E::term(), R::term()).
detach(Name, _GearBox, _Gear, ID) ->
    gen_server:call(format_name(Name), #detach{id=ID}).

-record(overload, {load::term()}).

-spec overload(Name::serial_no(), GearBox::assembly(), Gear::assembly(), Load::term()) ->
                      Load::term().
overload(Name, _GearBox, _Gear, Load) ->
    Pid = syn:whereis(Name),
    erlang:send(Pid, #overload{load=Load}), 
    Load.

-record(block, {part::assembly(), failure::term()}).

-spec block(Name::serial_no(), GearBox::assembly(), Gear::assembly(), Part::assembly(), Failure::term()) -> 
                   Failure::term().
block(Name, _GearBox, _Gear, Part, Failure) ->
    Pid = syn:whereis(Name),
    erlang:send(Pid, #block{part=Part, failure=Failure}), 
    Failure.

-record(replace, {repair::assembly()}).

-spec replace(Name::serial_no(), GearBox::assembly(), Gear::assembly(), Repair::assembly()) -> 
                     success(Release::assembly()) | failure(E::term(), R::term()).
replace(Name, _GearBox, _Gear, Repair) ->
    gen_server:call(format_name(Name), #replace{repair=Repair}).

-record(rotate, {motion::term()}).

-spec rotate(Name::serial_no(), GearBox::assembly(), Gear::assembly(), Motion::term()) -> 
                    Motion::term().
rotate(Name, _GearBox, _Gear, Motion) ->
    Pid = syn:whereis(Name),
    erlang:send(Pid, #rotate{motion=Motion}), 
    Motion.

-record(transmit, {motion::term()}).

-spec transmit(Name::serial_no(), GearBox::assembly(), Gear::assembly(), Motion::term()) ->
                      Force::term().
transmit(Name, _GearBox, _Gear, Motion) ->
    gen_server:call(format_name(Name), #transmit{motion=Motion}).

-spec uninstall(Name::serial_no(), GearBox::assembly(), Gear::assembly(), Reason::term()) ->
                       ok.
uninstall(Name, _GearBox, _Gear, Reason) ->
    gen_server:stop(format_name(Name), Reason).

-record(accept, {criteria::acceptance_criteria()}).

-spec accept(Name::serial_no(), GearBox::assembly(), Gear::assembly(), Criteria::criteria()) ->
                    success() | failure(E::term(), R::term(), S::term()).
accept(Name, _GearBox, _Gear, Criteria) -> 
    gen_server:call(format_name(Name), #accept{criteria=Criteria}).

%% gen_server.
-record(state, {gearbox::assembly(), gear::assembly()}).

init(#install{gearbox=GearBox, gear=Gear, options=Opt}) ->
    Flags = proplists:get_value(process_flags, Opt, []),
    [process_flag(ID, Param)|| {ID, Param} <- [{trap_exit, true}|Flags]],
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
    {reply, {ok, Result}, State#state{gear=Release}};

handle_call(#accept{criteria = Criteria}, _From, #state{gearbox=GearBox, gear=Gear} = State) ->
    Status = erlmachine_gear:accept(GearBox, Gear, Criteria),
    {reply, Status, State};

handle_call(Req, _From,  #state{gearbox=GearBox, gear=Gear}=State) ->
    erlmachine_gear:call(GearBox, Gear, Req),
    {reply, ignored, State}.

handle_cast(Message, #state{gearbox=GearBox, gear=Gear}=State) ->
    erlmachine_gear:cast(GearBox, Gear, Message),
    {noreply, State}.

handle_info(#rotate{motion = Motion}, #state{gearbox=GearBox, gear=Gear}=State) ->
    {ok, Release} = erlmachine_gear:rotate(GearBox, Gear, Motion),
    {noreply, State#state{gear=Release}};

handle_info(#overload{load = Load}, #state{gearbox=GearBox, gear=Gear}=State) ->
    {ok, Release} = erlmachine_gear:overload(GearBox, Gear, Load),
    {noreply, State#state{gear=Release}};

handle_info(#block{part=Part, failure = Failure}, #state{gearbox=GearBox, gear=Gear}=State) ->
    {ok, Release} = erlmachine_gear:block(GearBox, Gear, Part, Failure),
    {noreply, State#state{gear=Release}};

handle_info(Load, #state{gearbox=GearBox, gear=Gear}=State) ->
    {ok, Release} = erlmachine_gear:load(GearBox, Gear, Load),
    {noreply, State#state{gear=Release}}.

terminate(Reason, #state{gearbox=GearBox, gear=Gear}) ->
    erlmachine_gear:uninstall(GearBox, Gear, Reason),
    ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

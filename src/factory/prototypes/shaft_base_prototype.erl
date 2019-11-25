-module(shaft_base_prototype).

-folder(<<"erlmachine/factory/prototypes/shaft_base_prototype">>).

%% I guess factory will write to catalogue via catalogue behaviour;
%% The main purpose of a prototype is to provide implemetation of both communication and configuration layers;
%% The main purpose of a detail is to provide mechanical intraface API over internal isolated product structure;
%% The main purpose of a model is to provide mechanical reflection of modelling process over whole assembly;

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
         attach/4, detach/4,
         overload/4, block/5, 
         replace/4,
         transmit/4, rotate/4,
         uninstall/4,
         accept/4
        ]).

-include("erlmachine_factory.hrl").
-include("erlmachine_system.hrl").

%% API.

-spec name() -> Name::atom().
name() ->
    ?MODULE.

format_name(SerialNumber) ->
    ID = erlang:binary_to_atom(SerialNumber, latin1),
    ID.

-record(install, {gearbox::assembly(), shaft::assembly(), options::list(tuple())}).

-spec install(Name::serial_no(), GearBox::assembly(), Shaft::assembly(), Options::list(tuple())) -> 
                     success(pid()) | ingnore | failure(E::term()).
install(Name, GearBox, Shaft, Options) ->
    ID = {local, format_name(Name)},
    gen_server:start_link(ID, ?MODULE, #install{gearbox=GearBox, shaft=Shaft, options=Options}, []).

%% I think about ability to reflect both kind of switching - manual and automated;
-record(attach, {part::assembly()}).

-spec attach(Name::serial_no(), GearBox::assembly(), Shaft::assembly(), Part::assembly()) -> 
                    success(Release::assembly()) | failure(E::term(), R::term()).
attach(Name, _GearBox, _Shaft, Part) ->
    gen_server:call(format_name(Name), #attach{part = Part}).

%% I think about ability to reflect both kind of switching - manual and automated;
-record(detach, {id::serial_no()}).

-spec detach(Name::serial_no(), GearBox::assembly(), Shaft::assembly(), ID::serial_no()) -> 
                    success(Release::assembly()) | failure(E::term(), R::term()).
detach(Name, _GearBox, _Shaft, ID) ->
    gen_server:call(format_name(Name), #detach{id=ID}).

-record(transmit, {motion::term()}).

-spec transmit(Name::serial_no(), GearBox::assembly(), Shaft::assembly(), Motion::term()) ->
                      success(Result::term()) | failure(E::term(), R::term()).
transmit(Name, _GearBox, _Shaft, Motion) ->
    gen_server:call(format_name(Name), #transmit{motion=Motion}).

-record(overload, {load::term()}).

-spec overload(Name::serial_no(), GearBox::assembly(), Shaft::assembly(), Load::term()) ->
                      Load::term().
overload(Name, _GearBox, _Shaft, Load) ->
    erlang:send(format_name(Name), #overload{load=Load}), 
    Load.

-record(block, {part::assembly(), failure::term()}).

-spec block(Name::serial_no(), GearBox::assembly(), Shaft::assembly(), Part::assembly(), Failure::term()) -> 
                   Failure::term().
block(Name, _GearBox, _Shaft, Part, Failure) ->
    erlang:send(format_name(Name), #block{part=Part, failure=Failure}), 
    Failure.

-record(replace, {repair::assembly()}).

-spec replace(Name::serial_no(), GearBox::assembly(), Shaft::assembly(), Repair::assembly()) -> 
                     success(Release::assembly()) | failure(E::term(), R::term()).
replace(Name, _GearBox, _Shaft, Repair) ->
    gen_server:call(format_name(Name), #replace{repair=Repair}).

-record(rotate, {motion::term()}).

-spec rotate(Name::serial_no(), GearBox::assembly(), Shaft::assembly(), Motion::term()) -> 
                    Motion::term().
rotate(Name, _GearBox, _Shaft, Motion) ->
    erlang:send(format_name(Name), #rotate{motion=Motion}), 
    Motion.

-spec uninstall(Name::serial_no(), GearBox::assembly(), Shaft::assembly(), Reason::term()) ->
                       ok.
uninstall(Name, _GearBox, _Shaft, Reason) ->
    gen_server:stop({local, format_name(Name)}, Reason).

-record(accept, {criteria::acceptance_criteria()}).

-spec accept(Name::serial_no(), GearBox::assembly(), Shaft::assembly(), Criteria::acceptance_criteria()) ->
                    accept() | reject().
accept(Name, _GearBox, _Shaft, Criteria) -> 
    gen_server:call(Name, #accept{criteria=Criteria}).

%% gen_server.
-record(state, {gearbox::assembly(), shaft::assembly()}).

init(#install{gearbox=GearBox, shaft=Shaft, options=Options}) ->
    [process_flag(ID, Param)|| {ID, Param} <- Options],
    %% process_flag(trap_exit, true), Needs to be passed by default;
    %% Gearbox is intended to use like specification of destination point (it's not about persistence);
    {ok, Release} = erlmachine_shaft:install(GearBox, Shaft),
    {ok, #state{gearbox=GearBox, shaft=Release}}.

handle_call(#attach{part = Part}, _From, #state{gearbox=GearBox, shaft=Shaft} = State) ->
    Result = {ok, Release} = erlmachine_shaft:attach(GearBox, Shaft, Part),
    {reply, Result, State#state{shaft=Release}};

handle_call(#detach{id = ID}, _From, #state{gearbox=GearBox, shaft=Shaft} = State) ->
    Result = {ok, Release} = erlmachine_shaft:detach(GearBox, Shaft, ID),
    {reply, Result, State#state{shaft=Release}};

handle_call(#transmit{motion = Motion}, _From, #state{gearbox=GearBox, shaft=Shaft} = State) ->
    {ok, Result, Release} = erlmachine_shaft:transmit(GearBox, Shaft, Motion),
    {reply, Result, State#state{shaft=Release}};

handle_call(#replace{repair=Repair}, _From, #state{gearbox=GearBox, shaft=Shaft} = State) ->
    Result = {ok, Release} = erlmachine_shaft:replace(GearBox, Shaft, Repair),
    {reply, Result, State#state{shaft=Release}};

handle_call(#accept{criteria = Criteria}, _From, #state{gearbox=GearBox, shaft=Shaft} = State) ->
    {ok, Report, Release} = erlmachine_shaft:accept(GearBox, Shaft, Criteria),
    {reply, Report, State#state{shaft=Release}};

handle_call(Req, _From, #state{gearbox=GearBox, shaft=Shaft} = State) ->
    erlmachine_shaft:call(GearBox, Shaft, Req),
    {reply, ignored, State}.

handle_cast(Message, #state{gearbox=GearBox, shaft=Shaft} = State) ->
    erlmachine_shaft:cast(GearBox, Shaft, Message),
    {noreply, State}.

handle_info(#rotate{motion = Motion}, #state{gearbox=GearBox, shaft=Shaft} = State) ->
    %% At that place we can adress rotated part by SN; 
    %% In that case all parts will be rotated by default;
    %% If you need to provide measurements is's suitable place for that;
    {ok, Release} = erlmachine_shaft:rotate(GearBox, Shaft, Motion),
    %% Potentially clients can provide sync delivery inside this call;
    %% It can work a very similar to job queue);
    {noreply, State#state{shaft=Release}};

handle_info(#overload{load = Load}, #state{gearbox=GearBox, shaft=Shaft} = State) ->
    {ok, Release} = erlmachine_shaft:overload(GearBox, Shaft, Load),
    {noreply, State#state{shaft=Release}};

handle_info(#block{part=Part, failure = Failure}, #state{gearbox=GearBox, shaft=Shaft} = State) ->
    %% Damage, Crash and Failure will be translated to specialized system gears;
    %% This produced stream can be consumed by custom components which can be able to provide repair;
    {ok, Release} = erlmachine_shaft:block(GearBox, Shaft, Part, Failure),
    {noreply, State#state{shaft=Release}};

handle_info(_Message, #state{gearbox=_GearBox, shaft=_Shaft} = State) ->
    %% We need to provide logging at that place;
    {noreply, State}.

%% When reason is different from normal, or stop - the broken part event is occured;
terminate(Reason, #state{gearbox=GearBox, shaft=Shaft}) ->
    erlmachine_shaft:uninstall(GearBox, Shaft, Reason),
    ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

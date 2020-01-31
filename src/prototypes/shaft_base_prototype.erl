-module(shaft_base_prototype).

-folder(<<"erlmachine/prototypes/shaft_base_prototype">>).

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
         attach/5, detach/4,
         overload/4, block/5, 
         replace/4,
         transmit/4, rotate/4,
         uninstall/4,
         accept/4
        ]).

-export([form/3, submit/4]).

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
install(Name, GearBox, Shaft, Opt) ->
    ID = {local, format_name(Name)},
    Command = #install{ gearbox=GearBox, shaft=Shaft, options=Opt },

    gen_server:start_link(ID, ?MODULE, Command, []).

%% I think about ability to reflect both kind of switching - manual and automated;
-record(attach, { extension::assembly(), register::term() }).

-spec attach(Name::serial_no(), GearBox::assembly(), Shaft::assembly(), Reg::term(), Ext::assembly()) -> 
                    success(assembly()) | failure(term(), term()).
attach(Name, _GearBox, _Shaft, Reg, Ext) ->
    Command = #attach{ extension=Ext, register=Reg },

    gen_server:call(format_name(Name), Command).

%% I think about ability to reflect both kind of switching - manual and automated;
-record(detach, {id::serial_no()}).

-spec detach(Name::serial_no(), GearBox::assembly(), Shaft::assembly(), ID::serial_no()) -> 
                    success(Release::assembly()) | failure(E::term(), R::term()).
detach(Name, _GearBox, _Shaft, ID) ->
    Command = #detach{ id=ID },

    gen_server:call(format_name(Name), Command).

-record(transmit, {motion::term()}).

-spec transmit(Name::serial_no(), GearBox::assembly(), Shaft::assembly(), Motion::term()) ->
                      success(Result::term()) | failure(E::term(), R::term()).
transmit(Name, _GearBox, _Shaft, Motion) ->
    Command = #transmit{ motion=Motion },

    gen_server:call(format_name(Name), Command).

-record(overload, {load::term()}).

-spec overload(Name::serial_no(), GearBox::assembly(), Shaft::assembly(), Load::term()) ->
                      Load::term().
overload(Name, _GearBox, _Shaft, Load) ->
    Command = #overload{ load=Load },

    erlang:send(format_name(Name), Command),
    Load.

-record(block, {part::assembly(), failure::term()}).

-spec block(Name::serial_no(), GearBox::assembly(), Shaft::assembly(), Part::assembly(), Failure::term()) -> 
                   Failure::term().
block(Name, _GearBox, _Shaft, Part, Failure) ->
    Command = #block{ part=Part, failure=Failure },

    erlang:send(format_name(Name), Command), 
    Failure.

-record(replace, {repair::assembly()}).

-spec replace(Name::serial_no(), GearBox::assembly(), Shaft::assembly(), Repair::assembly()) -> 
                     success(Release::assembly()) | failure(E::term(), R::term()).
replace(Name, _GearBox, _Shaft, Repair) ->
    Command = #replace{ repair=Repair },

    gen_server:call(format_name(Name), Command).

-record(rotate, {motion::term()}).

-spec rotate(Name::serial_no(), GearBox::assembly(), Shaft::assembly(), Motion::term()) -> 
                    Motion::term().
rotate(Name, _GearBox, _Shaft, Motion) ->
    Command = #rotate{ motion=Motion },

    erlang:send(format_name(Name), Command), 
    Motion.

-record(accept, {criteria::acceptance_criteria()}).

-spec accept(Name::serial_no(), GearBox::assembly(), Shaft::assembly(), Criteria::criteria()) ->
                    success() | failure(E::term(), R::term(), S::term()).
accept(Name, _GearBox, _Shaft, Criteria) -> 
    Command = #accept{ criteria=Criteria },

    gen_server:call(format_name(Name), Command).

-spec uninstall(Name::serial_no(), GearBox::assembly(), Shaft::assembly(), Reason::term()) ->
                       ok.
uninstall(Name, _GearBox, _Shaft, Reason) ->
    gen_server:stop(format_name(Name), Reason).

-record(form, {}).

-spec form(Name::serial_no(), GearBox::assembly(), Shaft::assembly()) -> 
                  term().
form(Name, _GearBox, _Shaft) ->
    Command = #form{},

    gen_server:call(format_name(Name), Command).

-record(submit, { form::term() }).

-spec submit(Name::serial_no(), GearBox::assembly(), Shaft::assembly(), Form::term()) -> 
                    term().
submit(Name, _GearBox, _Shaft, Form) ->
    Command = #submit{ form=Form },

    gen_server:call(format_name(Name), Command).

%% gen_server.
-record(state, {gearbox::assembly(), shaft::assembly()}).

init(#install{gearbox=GearBox, shaft=Shaft, options=Opt}) ->
    Flags = proplists:get_value(process_flags, Opt, []),
    [process_flag(ID, Param)|| {ID, Param} <- [{trap_exit, true}|Flags]],
    %% process_flag(trap_exit, true), Needs to be passed by default;
    %% Gearbox is intended to use like specification of destination point (it's not about persistence);
    {ok, Rel} = erlmachine_shaft:install(GearBox, Shaft),
    erlmachine:success(#state{ gearbox=GearBox, shaft=Rel }).

handle_call(#attach{extension = Ext, register = Reg}, _From, #state{gearbox=GearBox, shaft=Shaft} = State) ->
    {ok, Part, Rel} = erlmachine_shaft:attach(GearBox, Shaft, Reg, Ext),
    {reply, erlmachine:success(Part, Rel), State#state{ shaft=Rel }};

handle_call(#detach{id = ID}, _From, #state{gearbox=GearBox, shaft=Shaft} = State) ->
    {ok, Rel} = erlmachine_shaft:detach(GearBox, Shaft, ID),
    {reply, erlmachine:success(Rel), State#state{ shaft=Rel }};

handle_call(#transmit{motion = Motion}, _From, #state{gearbox=GearBox, shaft=Shaft} = State) ->
    {ok, Res, Rel} = erlmachine_shaft:transmit(GearBox, Shaft, Motion),
    {reply, erlmachine:success(Res, Rel), State#state{ shaft=Rel }};

handle_call(#accept{criteria = Criteria}, _From, #state{gearbox=GearBox, shaft=Shaft} = State) ->
    {ok, Res, Rel} = erlmachine_shaft:accept(GearBox, Shaft, Criteria),
    {reply, erlmachine:success(Res, Rel), State};

handle_call(#replace{repair=Repair}, _From, #state{gearbox=GearBox, shaft=Shaft} = State) ->
    {ok, Rel} = erlmachine_shaft:replace(GearBox, Shaft, Repair),
    {reply, erlmachine:success(Rel), State#state{ shaft=Rel }};

handle_call(#form{}, _From, #state{ gearbox=GearBox, shaft=Shaft } = State) ->
    {ok, Res, Rel} = erlmachine_shaft:form(GearBox, Shaft),
    {reply, erlmachine:success(Res, Rel), State};

handle_call(#submit{ form=Form }, _From, #state{ gearbox=GearBox, shaft=Shaft } = State) ->
    {ok, Res, Rel} = erlmachine_shaft:submit(GearBox, Shaft, Form),
    {reply, erlmachine:success(Res, Rel), State#state{ shaft=Rel }};

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

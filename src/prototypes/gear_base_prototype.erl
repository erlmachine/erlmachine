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

%% erlmachine_assembly
-export([install/4, uninstall/4, attach/5, detach/4]).

%% erlmachine_factory
-export([accept/4]).

%% erlmachine_system
-export([form/3, submit/4, overload/4]).

%% erlmachine_transmission
-export([rotate/4, transmit/4]).

-include("erlmachine_factory.hrl").
-include("erlmachine_system.hrl").

%% API.

-spec name() -> atom().
name() ->
    ?MODULE.

-spec format_name(SN::serial_no()) -> 
                               atom().
format_name(SN) ->
    erlang:binary_to_atom(SN, latin1).

-record(install, { gearbox::assembly(), gear::assembly(), options::list() }).

-spec install(Name::serial_no(), GearBox::assembly(), Gear::assembly(), Opt::list()) -> 
                     success(pid()) | ingnore | failure(term()).
install(Name, GearBox, Gear, Opt) ->
    Com = #install{ gearbox=GearBox, gear=Gear, options=Opt },

    gen_server:start_link({local, format_name(Name)}, ?MODULE, Com, []).

-record(attach, { extension::assembly(), register::term() }).

-spec attach(Name::serial_no(), GearBox::assembly(), Gear::assembly(), Reg::term(), Ext::assembly()) -> 
                    success(assembly()) | failure(term(), term()).
attach(Name, _, _, Reg, Ext) ->
    Com = #attach{ extension=Ext, register=Reg },

    gen_server:call(format_name(Name), Com).

-record(detach, { id::term() }).

-spec detach(Name::serial_no(), GearBox::assembly(), Gear::assembly(), Id::term()) -> 
                    success() | failure(term(), term()).
detach(Name, _, _, Id) ->
    Com = #detach{ id=Id },

    gen_server:call(format_name(Name), Com).

-record(overload, { load::term() }).

-spec overload(Name::serial_no(), GearBox::assembly(), Gear::assembly(), Load::term()) ->
                      term().
overload(Name, _, _, Load) ->
    Com = #overload{ load=Load },

    erlang:send(format_name(Name), Com), 
    Load.

-record(rotate, { motion::term() }).

-spec rotate(Name::serial_no(), GearBox::assembly(), Gear::assembly(), Motion::term()) -> 
                    term().
rotate(Name, _, _, Motion) ->
    Com = #rotate{ motion=Motion },

    erlang:send(format_name(Name), Com), 
    Motion.

-record(transmit, { motion::term() }).

-spec transmit(Name::serial_no(), GearBox::assembly(), Gear::assembly(), Motion::term()) ->
                      success(term()) | failure(term(), term(), term()).
transmit(Name, _, _, Motion) ->
    Com = #transmit{ motion=Motion },

    gen_server:call(format_name(Name), Com).

-record(accept, {criteria::acceptance_criteria()}).

-spec accept(Name::serial_no(), GearBox::assembly(), Gear::assembly(), Criteria::criteria()) ->
                    success(term()) | failure(term(), term(), term()).
accept(Name, _, _, Criteria) -> 
    Com = #accept{ criteria=Criteria },

    gen_server:call(format_name(Name), Com).

-spec uninstall(Name::serial_no(), GearBox::assembly(), Gear::assembly(), Reason::term()) ->
                       success().
uninstall(Name, _, _, Reason) ->
    gen_server:stop(format_name(Name), Reason).

-record(form, {}).

-spec form(Name::serial_no(), GearBox::assembly(), Gear::assembly()) -> 
                  success(term()) | failure(term(), term(), term()).
form(Name, _, _) ->
    Com = #form{},

    gen_server:call(format_name(Name), Com).

-record(submit, { form::term() }).

-spec submit(Name::serial_no(), GearBox::assembly(), Gear::assembly(), Form::term()) -> 
                    success(term()) | failure(term(), term(), term()).
submit(Name, _, _, Form) ->
    Com = #submit{ form=Form },

    gen_server:call(format_name(Name), Com).

%% gen_server.
-record(state, { gearbox::assembly(), gear::assembly() }).

init(#install{ gearbox=GearBox, gear=Gear, options=Opt }) ->
    Flags = proplists:get_value(process_flags, Opt, []),
    [process_flag(ID, Param)|| {ID, Param} <- [{trap_exit, true}|Flags]],
    {ok, Rel} = erlmachine_gear:install(GearBox, Gear),
    erlmachine:success(#state{ gearbox=GearBox, gear=Rel }).

handle_call(#attach{extension = Ext, register = Reg}, _From, #state{gearbox=GearBox, gear=Gear} = State) ->
    {ok, Part, Rel} = erlmachine_gear:attach(GearBox, Gear, Reg, Ext),
    {reply, erlmachine:success(Part, Rel), State#state{ gear=Rel }};

handle_call(#detach{id = Id}, _From, #state{gearbox=GearBox, gear=Gear} = State) ->
    {ok, Rel} = erlmachine_gear:detach(GearBox, Gear, Id),
    {reply, erlmachine:success(Rel), State#state{ gear=Rel }};

handle_call(#transmit{motion = Motion}, _From, #state{gearbox=GearBox, gear=Gear} = State) ->
    {ok, Res, Rel} = erlmachine_gear:transmit(GearBox, Gear, Motion),
    {reply, erlmachine:success(Res), State#state{ gear=Rel }};

handle_call(#accept{criteria = Criteria}, _From, #state{gearbox=GearBox, gear=Gear} = State) ->
    {ok, Rel} = erlmachine_gear:accept(GearBox, Gear, Criteria),
    {reply, erlmachine:success(), State#state{ gear=Rel }};

handle_call(#form{}, _From, #state{ gearbox=GearBox, gear=Gear } = State) ->
    {ok, Res, Rel} = erlmachine_gear:form(GearBox, Gear),
    {reply, erlmachine:success(Res), State#state{ gear=Rel }};

handle_call(#submit{ form=Form }, _From, #state{ gearbox=GearBox, gear=Gear } = State) ->
    {ok, Res, Rel} = erlmachine_gear:submit(GearBox, Gear, Form),
    {reply, erlmachine:success(Res), State#state{ gear=Rel }};

handle_call(Req, _From,  #state{gearbox=GearBox, gear=Gear}=State) ->
    erlmachine_gear:call(GearBox, Gear, Req),
    {reply, ignored, State}.

handle_cast(Message, #state{gearbox=GearBox, gear=Gear}=State) ->
    erlmachine_gear:cast(GearBox, Gear, Message),
    {noreply, State}.

handle_info(#rotate{motion = Motion}, #state{gearbox=GearBox, gear=Gear}=State) ->
    {ok, Rel} = erlmachine_gear:rotate(GearBox, Gear, Motion),
    %% Potentially clients can provide sync delivery inside this call;
    %% It can work a very similar to job queue);
    {noreply, State#state{gear=Rel}};

handle_info(#overload{load = Load}, #state{gearbox=GearBox, gear=Gear}=State) ->
    {ok, Rel} = erlmachine_gear:overload(GearBox, Gear, Load),
    {noreply, State#state{gear=Rel}};

handle_info(Load, #state{gearbox=GearBox, gear=Gear}=State) ->
    %% At that place load capacity control can be achived;
    {ok, Rel} = erlmachine_gear:load(GearBox, Gear, Load),
    {noreply, State#state{gear=Rel}}.

%% When reason is different from normal, or stop - the broken part event is occured;
terminate(Reason, #state{gearbox=GearBox, gear=Gear}) ->
    erlmachine_gear:uninstall(GearBox, Gear, Reason),
    ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

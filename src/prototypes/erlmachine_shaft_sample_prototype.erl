-module(erlmachine_shaft_sample_prototype).

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

-record(install, { gearbox::assembly(), shaft::assembly(), options::list() }).

-spec install(Name::serial_no(), GearBox::assembly(), Shaft::assembly(), Opt::list()) -> 
                     success(pid()) | ingnore | failure(E::term()).
install(Name, GearBox, Shaft, Opt) ->
    Com = #install{ gearbox=GearBox, shaft=Shaft, options=Opt },

    gen_server:start_link({local, format_name(Name)}, ?MODULE, Com, []).

%% I think about ability to reflect both kind of switching - manual and automated;
-record(attach, { extension::assembly(), register::term() }).

-spec attach(Name::serial_no(), GearBox::assembly(), Shaft::assembly(), Reg::term(), Ext::assembly()) -> 
                    success(assembly()) | failure(term(), term()).
attach(Name, _, _, Reg, Ext) ->
    Com = #attach{ extension=Ext, register=Reg },

    gen_server:call(format_name(Name), Com).

%% I think about ability to reflect both kind of switching - manual and automated;
-record(detach, { id::term() }).

-spec detach(Name::serial_no(), GearBox::assembly(), Shaft::assembly(), Id::term()) -> 
                    success() | failure(term(), term()).
detach(Name, _, _, Id) ->
    Com = #detach{ id=Id },

    gen_server:call(format_name(Name), Com).

-record(overload, { load::term() }).

-spec overload(Name::serial_no(), GearBox::assembly(), Shaft::assembly(), Load::term()) ->
                      Load::term().
overload(Name, _, _, Load) ->
    Com = #overload{ load=Load },

    erlang:send(format_name(Name), Com),
    Load.

-record(rotate, { motion::term() }).

-spec rotate(Name::serial_no(), GearBox::assembly(), Shaft::assembly(), Motion::term()) -> 
                    Motion::term().
rotate(Name, _, _, Motion) ->
    Com = #rotate{ motion=Motion },

    erlang:send(format_name(Name), Com), 
    Motion.

-record(transmit, { motion::term() }).

-spec transmit(Name::serial_no(), GearBox::assembly(), Shaft::assembly(), Motion::term()) ->
                      success(term()) | failure(term(), term(), term()).
transmit(Name, _, _, Motion) ->
    Com = #transmit{ motion=Motion },

    gen_server:call(format_name(Name), Com).

-record(accept, { criteria::acceptance_criteria() }).

-spec accept(Name::serial_no(), GearBox::assembly(), Shaft::assembly(), Criteria::criteria()) ->
                    success(term()) | failure(term(), term(), term()).
accept(Name, _, _, Criteria) -> 
    Com = #accept{ criteria=Criteria },

    gen_server:call(format_name(Name), Com).

-spec uninstall(Name::serial_no(), GearBox::assembly(), Shaft::assembly(), Reason::term()) ->
                       success().
uninstall(Name, _, _, Reason) ->
    gen_server:stop(format_name(Name), Reason).

-record(form, {}).

-spec form(Name::serial_no(), GearBox::assembly(), Shaft::assembly()) -> 
                  success(term()) | failure(term(), term(), term()).
form(Name, _, _) ->
    Com = #form{},

    gen_server:call(format_name(Name), Com).

-record(submit, { form::term() }).

-spec submit(Name::serial_no(), GearBox::assembly(), Shaft::assembly(), Form::term()) -> 
                    success(term()) | failure(term(), term(), term()).
submit(Name, _, _, Form) ->
    Com = #submit{ form=Form },

    gen_server:call(format_name(Name), Com).

%% gen_server.
-record(state, { gearbox::assembly(), shaft::assembly() }).

init(#install{ gearbox=GearBox, shaft=Shaft, options=Opt }) ->
    Flags = proplists:get_value(process_flags, Opt, []),
    [process_flag(ID, Param)|| {ID, Param} <- [{trap_exit, true}|Flags]],
    {ok, Rel} = erlmachine_shaft:install(GearBox, Shaft),
    erlmachine:success(#state{ gearbox=GearBox, shaft=Rel }).

handle_call(#attach{extension = Ext, register = Reg}, _From, #state{gearbox=GearBox, shaft=Shaft} = State) ->
    {ok, Part, Rel} = erlmachine_shaft:attach(GearBox, Shaft, Reg, Ext),
    {reply, erlmachine:success(Part, Rel), State#state{ shaft=Rel }};

handle_call(#detach{id = Id}, _From, #state{gearbox=GearBox, shaft=Shaft} = State) ->
    {ok, Rel} = erlmachine_shaft:detach(GearBox, Shaft, Id),
    {reply, erlmachine:success(Rel), State#state{ shaft=Rel }};

handle_call(#transmit{motion = Motion}, _From, #state{gearbox=GearBox, shaft=Shaft} = State) ->
    {ok, Rel} = erlmachine_shaft:transmit(GearBox, Shaft, Motion),
    {reply, erlmachine:success(), State#state{ shaft=Rel }};

handle_call(#accept{criteria = Criteria}, _From, #state{gearbox=GearBox, shaft=Shaft} = State) ->
    {ok, Rel} = erlmachine_shaft:accept(GearBox, Shaft, Criteria),
    {reply, erlmachine:success(), State#state{ shaft=Rel }};

handle_call(#form{}, _From, #state{ gearbox=GearBox, shaft=Shaft } = State) ->
    {ok, Res, Rel} = erlmachine_shaft:form(GearBox, Shaft),
    {reply, erlmachine:success(Res), State#state{ shaft=Rel }};

handle_call(#submit{ form=Form }, _From, #state{ gearbox=GearBox, shaft=Shaft } = State) ->
    {ok, Res, Rel} = erlmachine_shaft:submit(GearBox, Shaft, Form),
    {reply, erlmachine:success(Res), State#state{ shaft=Rel }};

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
    Fun = 
        fun (Part, Acc) ->
                case erlmachine_shaft:rotate(GearBox, Acc, Part, Motion) of
                    {ok, Res, Ret}  ->
                        erlmachine_shaft:rotation(GearBox, Part, Res),
                        Ret;
                    {ok, Ret} ->
                        Ret;
                    {error, _, _, Ret} ->
                        %% TODO place for logging here;
                        Ret
                end
        end,
    Rel = lists:foldl(Fun, Shaft, erlmachine_shaft:parts(Shaft)),
    %% Potentially clients can provide sync delivery inside this call;
    %% It can work a very similar to job queue);
    {noreply, State#state{ shaft=Rel }};

handle_info(#overload{load = Load}, #state{gearbox=GearBox, shaft=Shaft} = State) ->
    {ok, Rel} = erlmachine_shaft:overload(GearBox, Shaft, Load),
    {noreply, State#state{shaft=Rel}};

handle_info(Load, #state{gearbox=GearBox, shaft=Shaft} = State) ->
    %% We need to provide logging at that place;
    Fun = 
        fun (Part, Acc) ->
                case erlmachine_shaft:load(GearBox, Acc, Part, Load) of
                    {ok, Res, Ret}  ->
                        erlmachine_shaft:rotation(GearBox, Part, Res),
                        Ret;
                    {ok, Ret} ->
                        Ret;
                    {error, _, _, Ret} ->
                        %% TODO place for logging here;
                        Ret
                end
        end,
    Rel = lists:foldl(Fun, Shaft, erlmachine_shaft:parts(Shaft)),
    {noreply, State#state{ shaft=Rel }}.

%% When reason is different from normal, or stop - the broken part event is occured;
terminate(Reason, #state{gearbox=GearBox, shaft=Shaft}) ->
    erlmachine_shaft:uninstall(GearBox, Shaft, Reason),
    ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

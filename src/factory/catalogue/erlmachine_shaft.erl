-module(erlmachine_shaft).

%% erlmachine_assembly
-export([install/2, uninstall/3, attach/4, detach/3]).

%% erlmachine_factory
-export([accept/3]).

%% erlmachine_system
-export([form/2, submit/3, overload/3]).

%% erlmachine_transmission
-export([transmit/3, load/3, rotate/4, rotation/3]).

-export([shaft/0]).

-export([parts/1, parts/2]).

-export([record_name/0, attributes/0]).

-include("erlmachine_factory.hrl").
-include("erlmachine_system.hrl").

%% erlmachine_assembly callbacks:
-callback install(Label::term(), Ids::list(term()), State::term(), Opt::term(), Env::list()) -> 
    success(term()) | failure(term(), term(), term()).

-callback uninstall(Label::term(), Reason::term(), State::term()) -> 
    success(term()) | failure(term(), term(), term()).

-callback attach(Label::term(), Reg::term(), Id::term(), State::term()) -> 
    success(term()) | failure(term(), term(), term()).

-callback detach(Label::term(), Id::term(), State::term()) -> 
    success(term()) | failure(term(), term(), term()).

%% erlmachine_factory callbacks:
-callback accept(Label::term(), Params::term(), State::term()) -> 
    success(term()) | failure(term(), term(), term()).

%% erlmachine_system callbacks:
-callback overload(Label::term(), Load::term(), State::term()) -> 
    success(term()) | failure(term(), term(), term()).

-callback form(Label::term(), State::term()) ->
    success(term(), term()) | failure(term(), term(), term()).

-callback submit(Label::term(), Form::term(), State::term()) ->
    success(term()) | failure(term(), term(), term()).

%% erlmachine_transmission callbacks:
-callback rotate(Label::term(), Id::term(), Motion::term(), State::term()) -> 
    success(term(), term()) | success(term()) | failure(term(), term(), term()).

-callback transmit(Label::term(), Motion::term(), State::term()) -> 
    success(term(), term()) | failure(term(), term(), term()).

-callback load(Label::term(), Load::term(), State::term()) ->
    success(term(), term()) |  success(term()) | failure(term(), term(), term()).

-optional_callbacks([attach/4, detach/3]).
-optional_callbacks([form/2, submit/3, overload/3]).

%% Instead of gear the main puropse of shaft is to transmit power between parts;

-record(shaft, {}).

-type shaft() :: #shaft{}.

-export_type([shaft/0]).

-spec record_name() -> atom().
record_name() ->
    shaft.

-spec attributes() -> list(atom()).
attributes() ->
    record_info(fields, shaft).

-spec shaft() -> shaft().
shaft() ->
    #shaft{ }.

-spec install(GearBox::assembly(), Shaft::assembly()) ->
                     success(term()) | failure(term(), term(), term()).
install(GearBox, Shaft) ->
    ModelName = erlmachine_assembly:model_name(Shaft),
    Label = erlmachine_assembly:label(Shaft),
 
    Env = erlmachine_gearbox:env(GearBox), 
    Opt = erlmachine_assembly:model_options(Shaft),
    %% We can check exported functions accordingly to this kind of behaviour; 
    %% We are going to add error handling later; 
    Ids = [erlmachine_assembly:label(Part)|| Part <- erlmachine_assembly:parts(Shaft)], 

    case ModelName:install(Label, Ids, state(Shaft), Opt, Env) of
        {ok, State} ->
            Rel = state(Shaft, State),
            erlmachine_assembly:installed(GearBox, Rel),
            erlmachine:success(Rel);
        {error, E, R, State} ->
            erlmachine:failure(E, R, state(Shaft, State))
    end.

-spec attach(GearBox::assembly(), Shaft::assembly(), Reg::term(), Ext::assembly()) ->
                    success(assembly(), term()) | failure(term(), term(), term()).
attach(GearBox, Shaft, Reg, Ext) ->
    ModelName= erlmachine_assembly:model_name(Shaft),
    Label = erlmachine_assembly:label(Shaft), Id = erlmachine_assembly:label(Ext),

    Mod = ModelName, Fun = attach, Args = [Label, Reg, Id, state(Shaft)],
    case erlmachine:optional_callback(Mod, Fun, Args, erlmachine:success(state(Shaft))) of
        {ok, State} ->
            Part = Ext,
            Rel = erlmachine_assembly:add(state(Shaft, State), Part),
            erlmachine_assembly:attached(GearBox, Rel, Part),
            erlmachine:success(Part, Rel);
        {error, E, R, State} ->
            erlmachine:failure(E, R, state(Shaft, State))
    end.

-spec detach(GearBox::assembly(), Shaft::assembly(), Id::serial_no()) ->
                    success(term()) | failure(term(), term(), term()).
detach(GearBox, Shaft, Id) ->
    ModelName= erlmachine_assembly:model_name(Shaft),
    Label = erlmachine_assembly:label(Shaft),

    Mod = ModelName, Fun = detach, Args = [Label, Id, state(Shaft)],
    case erlmachine:optional_callback(Mod, Fun, Args, erlmachine:success(state(Shaft))) of
        {ok, State} ->
            Rel = erlmachine_assembly:remove(state(Shaft, State), Id),
            erlmachine_assembly:detached(GearBox, Rel, Id),
            erlmachine:success(Rel);
        {error, E, R, State} ->
            erlmachine:failure(E, R, state(Shaft, State))
    end.

-spec accept(GearBox::assembly(), Shaft::assembly(), Criteria::term()) ->
                    success(term()) | failure(term(), term(), term()).
accept(GearBox, Shaft, Criteria) ->
    ModelName = erlmachine_assembly:model_name(Shaft),
    Label = erlmachine_assembly:label(Shaft),

    case ModelName:accept(Label, Criteria, state(Shaft)) of
        {ok, State} ->
            Rel = state(Shaft, State),
            erlmachine_factory:accepted(GearBox, Rel, Criteria),
            erlmachine:success(Rel);
        {error, E, R, State} ->
            Rel = state(Shaft, State),
            erlmachine_factory:rejected(GearBox, Rel, Criteria),
            erlmachine:failure(E, R, Rel)
    end.

-spec rotate(GearBox::assembly(), Shaft::assembly(), Ext::assembly(), Motion::term()) ->
                    success(term(), term()) | success(term()) | failure(term(), term(), term()).
rotate(GearBox, Shaft, Ext, Motion) ->
    ModelName = erlmachine_assembly:model_name(Shaft),
    Label = erlmachine:label(Shaft),

    case ModelName:rotate(Label, erlmachine:label(Ext), Motion, state(Shaft)) of 
        {ok, Res, State} ->
            erlmachine:success(Res, state(Shaft, State));
        {ok, State} ->
            erlmachine:success(state(Shaft, State));
        {error, E, R, State} ->
            Rel = state(Shaft, State),
            erlmachine_system:blocked(GearBox, Rel, E, R),
            erlmachine:failure(E, R, Rel)
    end.

-spec load(GearBox::assembly(), Shaft::assembly(), Load::term()) ->
                  success(term(), term()) | success(term()) | failure(term(), term(), term()).
load(GearBox, Shaft, Load) ->
    ModelName = erlmachine_assembly:model_name(Shaft),
    Label = erlmachine_assembly:label(Shaft),

    case ModelName:load(Label, Load, state(Shaft)) of
        {ok, Res, State} -> 
            erlmachine:success(Res, state(Shaft, State));
        {ok, State} -> 
            erlmachine:success(state(Shaft, State));
        {error, E, R, State} ->
            Rel = state(Shaft, State),
            erlmachine_system:blocked(GearBox, Rel, E, R),
            erlmachine:failure(E, R, Rel)
    end.

-spec transmit(GearBox::assembly(), Shaft::assembly(), Motion::term()) ->
                      success(term(), term()) | failure(term(), term(), term()).
transmit(_GearBox, Shaft, Motion) ->
    ModelName = erlmachine_assembly:model_name(Shaft),
    Label = erlmachine_assembly:label(Shaft),
 
    case ModelName:tranmsit(Label, Motion, state(Shaft)) of
        {ok, Res, Body} ->
            Rel = state(Shaft, Body),
            erlmachine:success(Res, Rel);
        {error, E, R, State} ->
            erlmachine:failure(E, R, state(Shaft, State))
    end.

-spec overload(GearBox::assembly(), Shaft::assembly(), Load::term()) ->
                      success(term()) | failure(term(), term(), term()).
overload(GearBox, Shaft, Load) ->
    ModelName = erlmachine_assembly:model_name(Shaft),
    Label = erlmachine_assembly:label(Shaft),

    Mod = ModelName, Fun = overload, Args = [Label, Load, state(Shaft)],
    {ok, State} = erlmachine:optional_callback(Mod, Fun, Args, erlmachine:success(state(Shaft))),

    Rel = state(Shaft, State),
    erlmachine_assembly:overloaded(GearBox, Rel, Load),
    erlmachine:success(Rel).

-spec uninstall(GearBox::assembly(), Shaft::assembly(), Reason::term()) -> 
                       success(term()).
uninstall(GearBox, Shaft, Reason) ->
    ModelName = erlmachine_assembly:model_name(Shaft),
    Label = erlmachine_assembly:label(Shaft),
  
    {ok, State} = ModelName:uninstall(Label, Reason, state(Shaft)),
    
    Rel = state(Shaft, State),
    erlmachine_assembly:uninstalled(GearBox, Rel, Reason),
    erlmachine:success(Rel).

-spec form(GearBox::assembly(), Shaft::assembly()) ->
                  success(term(), term()) | failure(term(), term(), term()).
form(_GearBox, Shaft) ->
    ModelName = erlmachine_assembly:model_name(Shaft),
    Label = erlmachine_assembly:label(Shaft),

    Mod = ModelName, Fun = form, Args = [Label, state(Shaft)],
    Def = erlmachine:success([], state(Shaft)),

    case erlmachine:optional_callback(Mod, Fun, Args, Def) of
        {ok, Form, State} ->
            erlmachine:success(Form, state(Shaft, State));
        {error, E, R, State} ->
            erlmachine:failure(E, R, state(Shaft, State))
    end.

-spec submit(GearBox::assembly(), Shaft::assembly(), Form::term()) ->
                    success(term(), term()) | failure(term(), term(), term()).
submit(_GearBox, Shaft, Form) ->
    ModelName = erlmachine_assembly:model_name(Shaft),
    Label = erlmachine_assembly:label(Shaft),

    case ModelName:submit(Label, Form, state(Shaft)) of
        {ok, Res, State} ->
            erlmachine:success(Res, state(Shaft, State));
        {error, E, R, State} ->
            erlmachine:failure(E, R, state(Shaft, State))
    end.

-spec state(Shaft::assembly()) -> term().
state(Shaft) ->
    erlmachine_assembly:body(Shaft).

-spec state(Shaft::assembly(), State::term()) -> assembly().
state(Shaft, State) ->
    erlmachine_assembly:body(Shaft, State).

-spec parts(Shaft::assembly(), Parts::list(assembly())) -> assembly().
parts(Shaft, Parts) ->
    erlmachine_assembly:parts(Shaft, Parts).

-spec parts(Shaft::assembly()) -> list().
parts(Shaft) ->
    erlmachine_assembly:parts(Shaft).

-spec rotation(GearBox::assembly(), Part::assembly(), Motion::term()) -> term().
rotation(GearBox, Part, Motion) ->
    erlmachine_transmission:rotation(GearBox, Part, Motion).


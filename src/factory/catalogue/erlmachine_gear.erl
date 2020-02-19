-module(erlmachine_gear).

-export([
         install/2,
         attach/4, detach/3,
         replace/3,
         transmit/3, load/3, rotate/3,
         accept/3,
         overload/3, block/4,
         uninstall/3
        ]).

-export([form/2, submit/3]).

-export([gear/0]).

-export([parts/2]).

-export([record_name/0, attributes/0]).

-include("erlmachine_factory.hrl").
-include("erlmachine_system.hrl").

-callback install(Label::term(), Ids::list(), State::term(), Opt::term(), Env::list()) -> 
    success(term()) | failure(term(), term(), term()) | failure(term()).

-callback replace(Label::term(), Id::term(), State::term()) -> 
    success(term()) | failure(term(), term(), term()) | failure(term()).

-callback uninstall(Label::term(), Reason::term(), State::term()) -> 
    success(term()) | failure(term(), term(), term()) | failure(term()).

-callback accept(Label::term(), Criteria::term(), State::term()) -> 
    success() | failure(term(), term(), term()).

-callback attach(Label::term(), Reg::term(), Id::term(), State::term()) -> 
    success(term()) | failure(term(), term(), term()) | failure(term()).

-callback detach(Label::term(), Id::term(), State::term()) -> 
    success(term()) | failure(term(), term(), term()) | failure(term()).

-callback rotate(Label::term(), Motion::term(), State::term()) -> 
    success(term()) | success(term(), term()) | failure(term(), term(), term()) | failure(term()).

-callback transmit(Label::term(), Motion::term(), State::term()) -> 
    success(term(), term()) | failure(term(), term(), term()) | failure(term()).

-callback load(Label::term(), Load::term(), State::term()) ->
    success(term()) |  success(term(), term()) | failure(term(), term(), term()) | failure(term()).

-callback overload(Label::term(), Load::term(), State::term()) -> 
    success(term()) | failure(term(), term(), term()) | failure(term()).

-callback block(Label::term(), Id::term(), Failure::term(), State::term()) -> 
    success(term()) | failure(term(), term(), term()) | failure(term()).

-callback form(Label::term(), State::term()) ->
    success(term(), term()) | failure(term(), term(), term()) | failure(term()).

-callback submit(Label::term(), Form::term(), State::term()) ->
    success(term()) | failure(term(), term(), term()) | failure(term()).

-optional_callbacks([replace/3, attach/4, detach/3, overload/3, block/4]).
-optional_callbacks([form/2, submit/3]).

%% The main difference between gear and shaft in the next:
%% Gear as working element, shaft is transmitter instead; 

-record(gear, {}). %% TODO Here is place with additional attributes discribed product itself;

-type gear() :: #gear{}.

-export_type([gear/0]).

-spec record_name() -> atom().
record_name() ->
    gear.

-spec attributes() -> list(atom()).
attributes() ->
    record_info(fields, gear).

-spec gear() -> gear(). %% Default representation;
gear() ->
    #gear{}.

-spec install(GearBox::assembly(), Gear::assembly()) -> 
                     success(assembly()) | failure(term(), term(), term()).
install(GearBox, Gear) ->
    ModelName = erlmachine_assembly:model_name(Gear),
    Label = erlmachine_assembly:label(Gear),
    Env = erlmachine_gearbox:env(GearBox), 
    Opt = erlmachine_assembly:model_options(Gear),
    %% We can check exported functions accordingly to this kind of behaviour; 
    %% We are going to add error handling later; 
    Ids = [erlmachine_assembly:label(Part)|| Part <- erlmachine_assembly:parts(Gear)],

    {ok, State} = ModelName:install(Label, Ids, state(Gear), Opt, Env),
    
    %% We are going to add error handling later; 
    Rel = state(Gear, State), 
    erlmachine_assembly:installed(GearBox, Rel),
    {ok, Rel}.

-spec attach(GearBox::assembly(), Gear::assembly(), Reg::term(), Ext::assembly()) ->
                    success(assembly()) | failure(term(), term(), term()).
attach(GearBox, Gear, Reg, Ext) ->
    ModelName= erlmachine_assembly:model_name(Gear),
    Label = erlmachine_assembly:label(Gear), 
    Id = erlmachine_assembly:label(Ext),
 
    Mod = ModelName, Fun = attach, Args = [Label, Reg, Id, state(Gear)],
    {ok, State} = erlmachine:optional_callback(Mod, Fun, Args, erlmachine:success(state(Gear))),
    
    Part = Ext,
    %% TODO At this place we can provide modification layer over attach;
    Rel = erlmachine_assembly:parts(state(Gear, State), [Part]),
    erlmachine_assembly:attached(GearBox, Rel, Part),
    {ok, Part, Rel}.

-spec detach(GearBox::assembly(), Gear::assembly(), Id::serial_no()) ->
                    success(assembly()) | failure(term(), term(), term()).
detach(GearBox, Gear, Id) ->
    ModelName= erlmachine_assembly:model_name(Gear),
    Label = erlmachine_assembly:label(Gear),

    Mod = ModelName, Fun = detach, Args = [Label, Id, state(Gear)],
    {ok, State} = erlmachine:optional_callback(Mod, Fun, Args, erlmachine:success(state(Gear))),

    Rel = erlmachine_assembly:parts(state(Gear, State), []),
    erlmachine_assembly:detached(GearBox, Rel, Id),
    {ok, Rel}.

-spec replace(GearBox::assembly(), Gear::assembly(), Part::assembly()) ->
                     success(assembly()) | failure(term(), term(), term()).
replace(GearBox, Gear, Part) ->
    ModelName = erlmachine_assembly:model_name(Gear),
    Label = erlmachine_assembly:label(Gear), Id = erlmachine_assembly:label(Part),

    Mod = ModelName, Fun = replace, Args = [Label, Id, state(Gear)],
    {ok, State} = erlmachine:optional_callback(Mod, Fun, Args, erlmachine:success(state(Gear))),

    Rel = state(Gear, State),
    erlmachine_assembly:replaced(GearBox, Rel, Part),
    {ok, Rel}.

-spec accept(GearBox::assembly(), Gear::assembly(), Criteria::term()) ->
                    success() | failure(term(), term(), term()).
accept(GearBox, Gear, Criteria) ->
    ModelName = erlmachine_assembly:model_name(Gear),
    Label = erlmachine_assembly:label(Gear),

    {ok, Res, State} = ModelName:accept(Label, Criteria, state(Gear)),

    Rel = state(Gear, State),
    case Res of
        ok ->
            erlmachine_factory:accepted(GearBox, Rel, Criteria);
        _ ->
            erlmachine_factory:rejected(GearBox, Rel, Criteria, Res)
    end,
    {ok, Res, Gear}.

-spec rotate(GearBox::assembly(), Gear::assembly(), Motion::term()) ->
                  success(assembly()) | failure(term(), term(), term()).
rotate(GearBox, Gear, Motion) ->
    ModelName = erlmachine_assembly:model_name(Gear),
    Label = erlmachine_assembly:label(Gear),

    case ModelName:rotate(Label, Motion, state(Gear)) of 
        {ok, Res, State} -> 
            Parts = erlmachine_assembly:parts(Gear),
            [erlmachine_transmission:rotation(GearBox, Part, Res) || Part <- Parts],
            {ok, state(Gear, State)};
        {ok, State} ->
            {ok, state(Gear, State)}
        end.

-spec transmit(GearBox::assembly(), Gear::assembly(), Motion::term()) ->
                      success(term(), assembly()) | failure(term(), term(), term()).
transmit(_GearBox, Gear, Motion) ->
    ModelName = erlmachine_assembly:model_name(Gear),
    Label = erlmachine_assembly:label(Gear),
 
    {ok, Res, State} = ModelName:transmit(Label, Motion, state(Gear)),
    
    Rel = state(Gear, State),
    {ok, Res, Rel}.

-spec overload(GearBox::assembly(), Gear::assembly(), Load::term()) ->
                      success(assembly()) | failure(term(), term(), term()).
overload(GearBox, Gear, Load) ->
    ModelName = erlmachine_assembly:model_name(Gear),
    Label = erlmachine_assembly:label(Gear),

    Mod = ModelName, Fun = overload, Args = [Label, Load, state(Gear)],
    {ok, State} = erlmachine:optional_callback(Mod, Fun, Args, erlmachine:success(state(Gear))),

    Rel = state(Gear, State),
    erlmachine_assembly:overloaded(GearBox, Rel, Load),
    {ok, Rel}.

-spec block(GearBox::assembly(), Gear::assembly(), Part::assembly(), Failure::term()) ->
                      success(assembly()) | failure(term(), term(), term()).
block(GearBox, Gear, Part, Failure) ->
    ModelName = erlmachine_assembly:model_name(Gear),
    Label = erlmachine_assembly:label(Gear), Id = erlmachine_assembly:label(Part),

    Mod = ModelName, Fun = block, Args = [Label, Id, Failure, state(Gear)],
    {ok, State} = erlmachine:optional_callback(Mod, Fun, Args, erlmachine:success(state(Gear))),

    Rel = state(Gear, State),
    erlmachine_assembly:blocked(GearBox, Rel, Part, Failure),
    {ok, Rel}.

-spec load(GearBox::assembly(), Gear::assembly(), Load::term()) ->
                    success(assembly()) | failure(term(), term(), term()).
load(GearBox, Gear, Load) ->
    ModelName = erlmachine_assembly:model_name(Gear),
    Label = erlmachine_assembly:label(Gear),
    Parts = erlmachine_assembly:parts(Gear),

    case ModelName:load(Label, Load, state(Gear)) of 
        {ok, Res, State} -> 
            [erlmachine_transmission:rotation(GearBox, Part, Res) || Part <- Parts], 
            {ok, state(Gear, State)};
        {ok, State} -> 
            {ok, state(Gear, State)}
    end.

-spec uninstall(GearBox::assembly(), Gear::assembly(), Reason::term()) -> 
                       ok.
uninstall(GearBox, Gear, Reason) ->
    ModelName = erlmachine_assembly:model_name(Gear),
    Label = erlmachine_assembly:label(Gear),

    {ok, State} = ModelName:uninstall(Label, Reason, state(Gear)),

    Rel = state(Gear, State),
    erlmachine_assembly:uninstalled(GearBox, Rel, Reason),
    ok.

-spec form(GearBox::assembly(), Gear::assembly()) ->
                  success(term(), assembly()) | failure(term(), term(), term()).
form(_GearBox, Gear) ->
    ModelName = erlmachine_assembly:model_name(Gear),
    Label = erlmachine_assembly:label(Gear),

    Mod = ModelName, Fun = form, Args = [Label, state(Gear)],
    Def = erlmachine:success([], state(Gear)),

    {ok, Form, State} = erlmachine:optional_callback(Mod, Fun, Args, Def),
    {ok, Form, state(Gear, State)}.

-spec submit(GearBox::assembly(), Gear::assembly(), Form::term()) ->
                    success(term(), assembly()) | failure(term(), term(), term()).
submit(_GearBox, Gear, Form) ->
    ModelName = erlmachine_assembly:model_name(Gear),
    Label = erlmachine_assembly:label(Gear),

    {ok, Res, State} = ModelName:submit(Label, Form, state(Gear)),
    {ok, Res, state(Gear, State)}.

-spec state(Gear::assembly()) -> term().
state(Gear) ->
    erlmachine_assembly:body(Gear).

-spec state(Gear::assembly(), State::term()) -> assembly().
state(Gear, State) ->
    erlmachine_assembly:body(Gear, State).

-spec parts(Gear::assembly(), Parts::list(assembly())) -> assembly().
parts(Gear, [_]=Parts) ->
    erlmachine_assembly:parts(Gear, Parts).

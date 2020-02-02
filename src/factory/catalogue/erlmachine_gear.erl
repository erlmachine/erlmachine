-module(erlmachine_gear).

-export([
         install/2,
         attach/4, detach/3,
         replace/3,
         transmit/3, load/3, rotate/3,
         accept/3,
         overload/3, block/4,
         uninstall/3,
         form/2, submit/3
        ]).

-export([gear/0]).

-export([parts/2]).

-export([record_name/0, attributes/0]).

-include("erlmachine_factory.hrl").
-include("erlmachine_system.hrl").

-callback install(SN::serial_no(), ID::serial_no(), State::term(), Opt::term(), Env::list()) -> 
    success(term()) | failure(term(), term(), term()) | failure(term()).

-callback replace(SN::serial_no(), ID::serial_no(), State::term()) -> 
    success(term()) | failure(term(), term(), term()) | failure(term()).

-callback uninstall(SN::serial_no(), Reason::term(), State::term()) -> 
    success(term()) | failure(term(), term(), term()) | failure(term()).

-callback accept(SN::serial_no(), Criteria::term(), State::term()) -> 
    success() | failure(term(), term(), term()).

-callback attach(SN::serial_no(), Register::term(), ID::serial_no(), State::term()) -> 
    success(term()) | failure(term(), term(), term()) | failure(term()).

-callback detach(SN::serial_no(), ID::serial_no(), State::term()) -> 
    success(term()) | failure(term(), term(), term()) | failure(term()).

-callback rotate(SN::serial_no(), Motion::term(), State::term()) -> 
    success(term()) | success(term(), term()) | failure(term(), term(), term()) | failure(term()).

-callback transmit(SN::serial_no(), Motion::term(), State::term()) -> 
    success(term(), term()) | failure(term(), term(), term()) | failure(term()).

-callback load(SN::serial_no(), Load::term(), State::term()) ->
    success(term()) |  success(term(), term()) | failure(term(), term(), term()) | failure(term()).

-callback overload(SN::serial_no(), Load::term(), State::term()) -> 
    success(term()) | failure(term(), term(), term()) | failure(term()).

-callback block(SN::serial_no(), ID::serial_no(), Failure::term(), State::term()) -> 
    success(term()) | failure(term(), term(), term()) | failure(term()).

-callback form(SN::serial_no(), State::term()) ->
    success(term(), term()) | failure(term(), term(), term()) | failure(term()).

-callback submit(SN::serial_no(), Form::term(), State::term()) ->
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
    SN = erlmachine_assembly:serial_no(Gear),
    Env = erlmachine_gearbox:env(GearBox), 
    Opt = erlmachine_assembly:model_options(Gear),
    %% We can check exported functions accordingly to this kind of behaviour; 
    %% We are going to add error handling later; 
    Parts = erlmachine_assembly:parts(Gear), 
    ID = case 
             Parts of [] -> 
                 undefined;
             [Part] -> 
                 erlmachine_assembly:serial_no(Part)
         end,

    {ok, State} = ModelName:install(SN, ID, state(Gear), Opt, Env),
    
    %% We are going to add error handling later; 
    Rel = state(Gear, State), 
    erlmachine_assembly:installed(GearBox, Rel),
    {ok, Rel}.

-spec attach(GearBox::assembly(), Gear::assembly(), Register::term(), Extension::assembly()) ->
                    success(assembly()) | failure(term(), term(), term()).
attach(GearBox, Gear, Register, Extension) ->
    ModelName= erlmachine_assembly:model_name(Gear),
    SN = erlmachine_assembly:serial_no(Gear), 
    ID = erlmachine_assembly:serial_no(Extension),
 
    Mod = ModelName, Fun = attach, Args = [SN, Register, ID, state(Gear)],
    {ok, State} = erlmachine:optional_callback(Mod, Fun, Args, erlmachine:success(state(Gear))),
    
    Part = Extension,
    %% TODO At this place we can provide modification layer over attach;
    Rel = erlmachine_assembly:parts(state(Gear, State), [Part]),
    erlmachine_assembly:attached(GearBox, Rel, Part),
    {ok, Part, Rel}.

-spec detach(GearBox::assembly(), Gear::assembly(), ID::serial_no()) ->
                    success(assembly()) | failure(term(), term(), term()).
detach(GearBox, Gear, ID) ->
    ModelName= erlmachine_assembly:model_name(Gear),
    SN = erlmachine_assembly:serial_no(Gear),

    Mod = ModelName, Fun = detach, Args = [SN, ID, state(Gear)],
    {ok, State} = erlmachine:optional_callback(Mod, Fun, Args, erlmachine:success(state(Gear))),

    Rel = erlmachine_assembly:parts(state(Gear, State), []),
    erlmachine_assembly:detached(GearBox, Rel, ID),
    {ok, Rel}.

-spec replace(GearBox::assembly(), Gear::assembly(), Part::assembly()) ->
                     success(assembly()) | failure(term(), term(), term()).
replace(GearBox, Gear, Part) ->
    ModelName = erlmachine_assembly:model_name(Gear),
    SN = erlmachine_assembly:serial_no(Gear), ID = erlmachine_assembly:serial_no(Part),

    Mod = ModelName, Fun = replace, Args = [SN, ID, state(Gear)],
    {ok, State} = erlmachine:optional_callback(Mod, Fun, Args, erlmachine:success(state(Gear))),

    Rel = state(Gear, State),
    erlmachine_assembly:replaced(GearBox, Rel, Part),
    {ok, Rel}.

-spec accept(GearBox::assembly(), Gear::assembly(), Criteria::term()) ->
                    success() | failure(term(), term(), term()).
accept(GearBox, Gear, Criteria) ->
    ModelName = erlmachine_assembly:model_name(Gear),
    SN = erlmachine_assembly:serial_no(Gear),

    {ok, Res, State} = ModelName:accept(SN, Criteria, state(Gear)),

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
    SN = erlmachine_assembly:serial_no(Gear),
    Parts =erlmachine_assembly:parts(Gear),

    case ModelName:rotate(SN, Motion, state(Gear)) of 
        {ok, Res, State} -> 
            [erlmachine_transmission:rotate(GearBox, Part, Res) || Part <- Parts],
            {ok, state(Gear, State)};
        {ok, State} ->
            {ok, state(Gear, State)}
        end.

-spec transmit(GearBox::assembly(), Gear::assembly(), Motion::term()) ->
                      success(term(), assembly()) | failure(term(), term(), term()).
transmit(_GearBox, Gear, Motion) ->
    ModelName = erlmachine_assembly:model_name(Gear),
    SN = erlmachine_assembly:serial_no(Gear),
 
    {ok, Res, State} = ModelName:transmit(SN, Motion, state(Gear)),
    
    Rel = state(Gear, State),
    {ok, Res, Rel}.

-spec overload(GearBox::assembly(), Gear::assembly(), Load::term()) ->
                      success(assembly()) | failure(term(), term(), term()).
overload(GearBox, Gear, Load) ->
    ModelName = erlmachine_assembly:model_name(Gear),
    SN = erlmachine_assembly:serial_no(Gear),

    Mod = ModelName, Fun = overload, Args = [SN, Load, state(Gear)],
    {ok, State} = erlmachine:optional_callback(Mod, Fun, Args, erlmachine:success(state(Gear))),

    Rel = state(Gear, State),
    erlmachine_assembly:overloaded(GearBox, Rel, Load),
    {ok, Rel}.

-spec block(GearBox::assembly(), Gear::assembly(), Part::assembly(), Failure::term()) ->
                      success(assembly()) | failure(term(), term(), term()).
block(GearBox, Gear, Part, Failure) ->
    ModelName = erlmachine_assembly:model_name(Gear),
    SN = erlmachine_assembly:serial_no(Gear), ID = erlmachine_assembly:serial_no(Part),

    Mod = ModelName, Fun = block, Args = [SN, ID, Failure, state(Gear)],
    {ok, State} = erlmachine:optional_callback(Mod, Fun, Args, erlmachine:success(state(Gear))),

    Rel = state(Gear, State),
    erlmachine_assembly:blocked(GearBox, Rel, Part, Failure),
    {ok, Rel}.

-spec load(GearBox::assembly(), Gear::assembly(), Load::term()) ->
                    success(assembly()) | failure(term(), term(), term()).
load(GearBox, Gear, Load) ->
    ModelName = erlmachine_assembly:model_name(Gear),
    SN = erlmachine_assembly:serial_no(Gear),
    Parts = erlmachine_assembly:parts(Gear),

    case ModelName:load(SN, Load, state(Gear)) of 
        {ok, Res, State} -> 
            [erlmachine_transmission:rotate(GearBox, Part, Res) || Part <- Parts], 
            {ok, state(Gear, State)};
        {ok, State} -> 
            {ok, state(Gear, State)}
    end.

-spec uninstall(GearBox::assembly(), Gear::assembly(), Reason::term()) -> 
                       ok.
uninstall(GearBox, Gear, Reason) ->
    ModelName = erlmachine_assembly:model_name(Gear),
    SN = erlmachine_assembly:serial_no(Gear),

    {ok, State} = ModelName:uninstall(SN, Reason, state(Gear)),

    Rel = state(Gear, State),
    erlmachine_assembly:uninstalled(GearBox, Rel, Reason),
    ok.

-spec form(GearBox::assembly(), Gear::assembly()) ->
                  success(term(), assembly()) | failure(term(), term(), term()).
form(_GearBox, Gear) ->
    ModelName = erlmachine_assembly:model_name(Gear),
    SN = erlmachine_assembly:serial_no(Gear),

    Mod = ModelName, Fun = form, Args = [SN, state(Gear)],
    Def = erlmachine:success([], state(Gear)),

    {ok, Form, State} = erlmachine:optional_callback(Mod, Fun, Args, Def),
    {ok, Form, state(Gear, State)}.

-spec submit(GearBox::assembly(), Gear::assembly(), Form::term()) ->
                    success(term(), assembly()) | failure(term(), term(), term()).
submit(_GearBox, Gear, Form) ->
    ModelName = erlmachine_assembly:model_name(Gear),
    SN = erlmachine_assembly:serial_no(Gear),

    {ok, Res, State} = ModelName:submit(SN, Form, state(Gear)),
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

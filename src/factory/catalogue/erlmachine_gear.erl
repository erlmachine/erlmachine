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

-export([gear/0]).

-export([parts/2]).

-export([record_name/0, attributes/0]).

-include("erlmachine_factory.hrl").
-include("erlmachine_system.hrl").

-callback install(SN::serial_no(), ID::serial_no(), Body::term(), Opt::term(), Env::list()) -> 
    success(term()) | failure(term(), term(), term()) | failure(term()).

-callback replace(SN::serial_no(), ID::serial_no(), Body::term()) -> 
    success(term()) | failure(term(), term(), term()) | failure(term()).

-callback uninstall(SN::serial_no(), Reason::term(), Body::term()) -> 
    success(term()) | failure(term(), term(), term()) | failure(term()).

-callback accept(SN::serial_no(), Criteria::term(), Body::term()) -> 
    success() | failure(term(), term(), term()).

-callback attach(SN::serial_no(), Register::term(), ID::serial_no(), Body::term()) -> 
    success(term()) | failure(term(), term(), term()) | failure(term()).

-callback detach(SN::serial_no(), ID::serial_no(), Body::term()) -> 
    success(term()) | failure(term(), term(), term()) | failure(term()).

-callback rotate(SN::serial_no(), Motion::term(), Body::term()) -> 
    success(term()) | success(term(), term()) | failure(term(), term(), term()) | failure(term()).

-callback transmit(SN::serial_no(), Motion::term(), Body::term()) -> 
    success(term(), term()) | failure(term(), term(), term()) | failure(term()).

-callback load(SN::serial_no(), Load::term(), Body::term()) ->
    success(term()) |  success(term(), term()) | failure(term(), term(), term()) | failure(term()).

-callback overload(SN::serial_no(), Load::term(), Body::term()) -> 
    success(term()) | failure(term(), term(), term()) | failure(term()).

-callback block(SN::serial_no(), ID::serial_no(), Failure::term(), Body::term()) -> 
    success(term()) | failure(term(), term(), term()) | failure(term()).

-callback form(SN::serial_no(), Body::term()) ->
    success(term(), term()) | failure(term(), term(), term()) | failure(term()).

-callback submit(SN::serial_no(), Form::term(), Body::term()) ->
    success(term()) | failure(term(), term(), term()) | failure(term()).

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
                     success(Release::assembly()) | failure(E::term(), R::term(), Rejected::assembly()).
install(GearBox, Gear) ->
    ModelName = erlmachine_assembly:model_name(Gear),
    SN = erlmachine_assembly:serial_no(Gear),
    Env = erlmachine_gearbox:env(GearBox), 
    Options = erlmachine_assembly:model_options(Gear),
    %% We can check exported functions accordingly to this kind of behaviour; 
    %% We are going to add error handling later; 
    Parts = erlmachine_assembly:parts(Gear), 
    ID = case 
             Parts of [] -> 
                 undefined;
             [Part] -> 
                 erlmachine_assembly:serial_no(Part)
         end,

    {ok, Body} = ModelName:install(SN, ID, body(Gear), Options, Env),
    
    %% We are going to add error handling later; 
    Release = body(Gear, Body), 
    erlmachine_assembly:installed(GearBox, Release),
    {ok, Release}.

-spec attach(GearBox::assembly(), Gear::assembly(), Register::term(), Extension::assembly()) ->
                    success(Release::assembly()) | failure(E::term(), R::term(), Rejected::assembly()).
attach(GearBox, Gear, Register, Extension) ->
    ModelName= erlmachine_assembly:model_name(Gear),
    SN = erlmachine_assembly:serial_no(Gear), ID = erlmachine_assembly:serial_no(Extension),
 
    {ok, Body} = ModelName:attach(SN, Register, ID, body(Gear)),
    
    Part = Extension,
    %% TODO At this place we can provide modification layer over attach;
    Release = erlmachine_assembly:parts(body(Gear, Body), [Part]),
    erlmachine_assembly:attached(GearBox, Release, Part),
    {ok, Part, Release}.

-spec detach(GearBox::assembly(), Gear::assembly(), ID::serial_no()) ->
                    success(Release::assembly()) | failure(E::term(), R::term(),  Rejected::assembly()).
detach(GearBox, Gear, ID) ->
    ModelName= erlmachine_assembly:model_name(Gear),
    SN = erlmachine_assembly:serial_no(Gear),
    %% At that place we need to find Part inside assembly by SN and transmit;

    {ok, Body} = ModelName:detach(SN, ID, body(Gear)),
    
    Release = erlmachine_assembly:parts(body(Gear, Body), []),
    erlmachine_assembly:detached(GearBox, Release, ID),
    {ok, Release}.

-spec replace(GearBox::assembly(), Gear::assembly(), Part::assembly()) ->
                     success(Release::assembly()) | failure(E::term(), R::term(), Rejected::assembly()).
replace(GearBox, Gear, Part) ->
    ModelName = erlmachine_assembly:model_name(Gear),
    SN = erlmachine_assembly:serial_no(Gear), ID = erlmachine_assembly:serial_no(Part),
 
    {ok, Body} = ModelName:replace(SN, ID, body(Gear)),
    
    Release = body(Gear, Body),
    erlmachine_assembly:replaced(GearBox, Release, Part),
    {ok, Release}.

-spec accept(GearBox::assembly(), Gear::assembly(), Criteria::term()) ->
                    success() | failure(E::term(), R::term(), S::term()).
accept(GearBox, Gear, Criteria) ->
    ModelName = erlmachine_assembly:model_name(Gear),
    SN = erlmachine_assembly:serial_no(Gear),

    {ok, Result, _} = ModelName:accept(SN, Criteria, body(Gear)),
    
    case Result of 
        ok ->
            erlmachine_factory:accepted(GearBox, Gear, Criteria);
        _ ->
            erlmachine_factory:rejected(GearBox, Gear, Criteria, Result)
    end,
    {ok, Result, Gear}.

-spec rotate(GearBox::assembly(), Gear::assembly(), Motion::term()) ->
                  success(Release::assembly()) | failure(E::term(), R::term(), Rejected::assembly()).
rotate(GearBox, Gear, Motion) ->
    ModelName = erlmachine_assembly:model_name(Gear),
    SN = erlmachine_assembly:serial_no(Gear),
    Parts =erlmachine_assembly:parts(Gear),

    ReleaseBody = 
        case ModelName:rotate(SN, Motion, body(Gear)) of 
            {ok, Result, Body} -> 
                [erlmachine_transmission:rotate(GearBox, Part, Result) || Part <- Parts],
                Body;
            {ok, Body} -> 
                Body 
        end,

    Release = body(Gear, ReleaseBody),
    {ok, Release}.

-spec transmit(GearBox::assembly(), Gear::assembly(), Motion::term()) ->
                      success(Result::term(), Release::assembly()) | failure(E::term(), R::term(), Rejected::assembly()).
transmit(_GearBox, Gear, Motion) ->
    ModelName = erlmachine_assembly:model_name(Gear),
    SN = erlmachine_assembly:serial_no(Gear),
 
    {ok, Result, Body} = ModelName:transmit(SN, Motion, body(Gear)),
    
    Release = body(Gear, Body),
    {ok, Result, Release}.

-spec overload(GearBox::assembly(), Gear::assembly(), Load::term()) ->
                      success(Release::assembly()) | failure(E::term(), R::term(), Reject::assembly()).
overload(GearBox, Gear, Load) ->
    ModelName = erlmachine_assembly:model_name(Gear),
    SN = erlmachine_assembly:serial_no(Gear),
  
    {ok, Body} = ModelName:overload(SN, Load, body(Gear)),
    
    Release = body(Gear, Body),
    erlmachine_assembly:overloaded(GearBox, Release, Load),
    {ok, Release}.

-spec block(GearBox::assembly(), Gear::assembly(), Part::assembly(), Failure::term()) ->
                      success(Release::assembly()) | failure(E::term(), R::term(), Reject::assembly()).
block(GearBox, Gear, Part, Failure) ->
    ModelName = erlmachine_assembly:model_name(Gear),
    SN = erlmachine_assembly:serial_no(Gear), ID = erlmachine_assembly:serial_no(Part),
  
    {ok, Body} = ModelName:block(SN, ID, Failure, body(Gear)),
    
    Release = body(Gear, Body),
    erlmachine_assembly:blocked(GearBox, Release, Part, Failure),
    {ok, Release}.

-spec load(GearBox::assembly(), Gear::assembly(), Load::term()) ->
                    success(Release::assembly()) | failure(E::term(), R::term(), Rejected::assembly()).
load(GearBox, Gear, Load) ->
    ModelName = erlmachine_assembly:model_name(Gear),
    SN = erlmachine_assembly:serial_no(Gear),
    Parts =erlmachine_assembly:parts(Gear),
  
    ReleaseBody = 
        case ModelName:load(SN, Load, body(Gear)) of 
            {ok, Result, Body} -> 
                [erlmachine_transmission:rotate(GearBox, Part, Result) || Part <- Parts],
                Body;
            {ok, Body} -> 
                Body 
        end,
  
    Release = body(Gear, ReleaseBody),
    {ok, Release}.

-spec uninstall(GearBox::assembly(), Gear::assembly(), Reason::term()) -> 
                       ok.
uninstall(GearBox, Gear, Reason) ->
    ModelName = erlmachine_assembly:model_name(Gear),
    SN = erlmachine_assembly:serial_no(Gear),
  
    {ok, Body} = ModelName:uninstall(SN, Reason, body(Gear)),
    
    Release = body(Gear, Body),
    erlmachine_assembly:uninstalled(GearBox, Release, Reason),
    ok.

-spec body(Gear::assembly()) -> Body::term().
body(Gear) ->
    erlmachine_assembly:body(Gear).

-spec body(Gear::assembly(), Body::term()) -> Release::assembly().
body(Gear, Body) ->
    erlmachine_assembly:body(Gear, Body).

-spec parts(Gear::assembly(), Parts::list(assembly())) -> Release::assembly().
parts(Gear, [_]=Parts) ->
    Release = erlmachine_assembly:parts(Gear, Parts),
    Release.

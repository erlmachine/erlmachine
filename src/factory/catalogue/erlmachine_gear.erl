-module(erlmachine_gear).

-export([
         install/2,
         replace/3,
         transmit/3, load/3, rotate/3,
         accept/3,
         overload/3, block/4,
         uninstall/3
        ]).

-export([
         gear/1,
         body/1, body/2
        ]).

-include("erlmachine_factory.hrl").
-include("erlmachine_system.hrl").

-callback install(SN::serial_no(), Body::term(), Options::term(), Env::list()) -> 
    success(term()) | failure(term(), term(), term()) | failure(term()).

-callback replace(SN::serial_no(), ID::serial_no(), Body::term()) -> 
    success(term()) | failure(term(), term(), term()) | failure(term()).

-callback uninstall(SN::serial_no(), Reason::term(), Body::term()) -> 
    success(term()) | failure(term(), term(), term()) | failure(term()).

-callback accept(SN::serial_no(), Criteria::term(), Body::term()) -> 
    success(term(), term()) | failure(term(), term(), term()) | failure(term()).

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

%% The main difference between gear and shaft in the next:
%% Gear as working element, shaft is transmitter instead; 

-record(gear, {body::term()}).

-type gear() :: #gear{}.

-export_type([gear/0]).

-spec gear(Body::term()) -> gear().
gear(Body) ->
    #gear{body=Body}.

-spec install(GearBox::assembly(), Gear::assembly()) -> 
                     success(Release::assembly()) | failure(E::term(), R::term(), Rejected::assembly()).
install(GearBox, Gear) ->
    ModelName = erlmachine_assembly:model_name(Gear),
    SN = erlmachine_assembly:serial_no(Gear),
    Env = erlmachine_gearbox:env(GearBox), 
    Options = erlmachine_assembly:model_options(Gear),
    %% We can check exported functions accordingly to this kind of behaviour; 
    %% We are going to add error handling later; 
    {ok, Body} = ModelName:install(SN, body(Gear), Options, Env),
    %% We are going to add error handling later; 
    Release = body(Gear, Body), 
    Mounted = erlmachine_assembly:mounted(Gear),
    (Mounted /= undefined) andalso (erlmachine_assembly:prototype_name(Mounted)):installed(SN, Mounted, Release),
    (Mounted == GearBox) orelse (erlmachine_assembly:prototype_name(GearBox)):installed(SN, GearBox, Release),
    {ok, Release}.

-spec replace(GearBox::assembly(), Gear::assembly(), Part::assembly()) ->
                     success(Release::assembly()) | failure(E::term(), R::term(), Rejected::assembly()).
replace(GearBox, Gear, Part) ->
    ModelName = erlmachine_assembly:model_name(Gear),
    SN = erlmachine_assembly:serial_no(Gear), ID = erlmachine_assembly:serial_no(Part),
    {ok, Body} = ModelName:replace(SN, ID, body(Gear)),
    Release = body(Gear, Body),
    (erlmachine_assembly:prototype_name(GearBox)):replaced(SN, GearBox, Release, Part),
    {ok, Release}.

-spec accept(GearBox::assembly(), Gear::assembly(), Criteria::term()) ->
                    success(Report::term(), Release::assembly())| failure(E::term(), R::term(), Rejected::assembly()).
accept(GearBox, Gear, Criteria) ->
    ModelName = erlmachine_assembly:model_name(Gear),
    SN = erlmachine_assembly:serial_no(Gear),
    {Tag, Result, Body} = ModelName:accept(SN, Criteria, body(Gear)),
    Release = body(Gear, Body),
    case Tag of 
        ok ->
            Report = Result,
            (erlmachine_assembly:prototype_name(GearBox)):accepted(SN, GearBox, Release, Criteria, Report),
            {ok, Result, Release};
        error ->
            {_, Report} = Result,
            (erlmachine_assembly:prototype_name(GearBox)):rejected(SN, GearBox, Release, Criteria, Report),
            {error, Result, Release} 
    end.

-spec rotate(GearBox::assembly(), Gear::assembly(), Motion::term()) ->
                  success(Release::assembly()) | failure(E::term(), R::term(), Rejected::assembly()).
rotate(_GearBox, Gear, Motion) ->
    ModelName = erlmachine_assembly:model_name(Gear),
    SN = erlmachine_assembly:serial_no(Gear),
    Parts =erlmachine_assembly:parts(Gear),
    ReleaseBody = 
        case ModelName:rotate(SN, Motion, body(Gear)) of 
            {ok, Result, Body} -> 
                [erlmachine_transmission:rotate(Part, Result) || Part <- Parts],
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
    {ok, Result, Body} = ModelName:tranmsit(SN, Motion, body(Gear)),
    Release = body(Gear, Body),
    {ok, Result, Release}.

-spec overload(GearBox::assembly(), Gear::assembly(), Load::term()) ->
                      success(Release::assembly()) | failure(E::term(), R::term(), Reject::assembly()).
overload(GearBox, Gear, Load) ->
    ModelName = erlmachine_assembly:model_name(Gear),
    SN = erlmachine_assembly:serial_no(Gear),
    {ok, Body} = ModelName:overload(SN, Load, body(Gear)),
    Release = body(Gear, Body),
    (erlmachine_assembly:prototype_name(GearBox)):overloaded(SN, GearBox, Release, Load),
    {ok, Release}.

-spec block(GearBox::assembly(), Gear::assembly(), Part::assembly(), Failure::term()) ->
                      success(Release::assembly()) | failure(E::term(), R::term(), Reject::assembly()).
block(GearBox, Gear, Part, Failure) ->
    ModelName = erlmachine_assembly:model_name(Gear),
    SN = erlmachine_assembly:serial_no(Gear), ID = erlmachine_assembly:serial_no(Part),
    {ok, Body} = ModelName:block(SN, ID, Failure, body(Gear)),
    Release = body(Gear, Body),
    (erlmachine_assembly:prototype_name(GearBox)):blocked(SN, GearBox, Release, Part, Failure),
    {ok, Release}.

-spec load(GearBox::assembly(), Gear::assembly(), Load::term()) ->
                    success(Release::assembly()) | failure(E::term(), R::term(), Rejected::assembly()).
load(_GearBox, Gear, Load) ->
    ModelName = erlmachine_assembly:model_name(Gear),
    SN = erlmachine_assembly:serial_no(Gear),
    Parts =erlmachine_assembly:parts(Gear),
    ReleaseBody = 
        case ModelName:load(SN, Load, body(Gear)) of 
            {ok, Result, Body} -> 
                [erlmachine_transmission:rotate(Part, Result) || Part <- Parts],
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
    Mounted = erlmachine_assembly:mounted(Gear),
    (Mounted /= undefined) andalso (erlmachine_assembly:prototype_name(Mounted)):uninstalled(SN, Mounted, Release, Reason),
    (Mounted == GearBox) orelse (erlmachine_assembly:prototype_name(GearBox)):uninstalled(SN, GearBox, Release, Reason),
    ok.

-spec body(Gear::assembly()) -> Body::term().
body(Gear) ->
    Product = erlmachine_assembly:product(Gear),
    Product#gear.body.

-spec body(Gear::assembly(), Body::term()) -> Release::assembly().
body(Gear, Body) ->
    Product = erlmachine_assembly:product(Gear),
    erlmachine_assembly:product(Gear, Product#gear{body=Body}).

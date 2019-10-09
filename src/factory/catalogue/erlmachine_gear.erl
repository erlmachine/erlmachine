-module(erlmachine_gear).

-export([
         install/2,
         switch/3,
         replace/3,
         transmit/4, rotate/4,
         call/3, cast/3, info/3,
         accept/3,
         overload/3, block/4,
         uninstall/3
        ]).

-export([
         parts/1, parts/2, 
         body/1, body/2,
         mount/1, mount/2
        ]).

-include("erlmachine_factory.hrl").
-include("erlmachine_system.hrl").

%% The main difference between gear and shaft in the next:
%% Gear as working element, shaft is transmitter instead; 

-record(gear, {body::term(), parts::assembly(), mount::assembly()}).

-type gear() :: #gear{}.

-export_type([gear/0]).

-spec install(GearBox::assembly(), Gear::assembly()) -> 
                     success(Release::assembly()) | failure(E::term(), R::term(), Rejected::assembly()).
install(GearBox, Gear) ->
    Mount = mount(Gear),
    {ok, Body} = erlmachine_assembly:install_model(Gear),
    %% We are going to add error handling later; 
    Release = body(Gear, Body),
    (Mount /= undefined) andalso erlmachine_assembly:installed(Mount, Release),
    (Mount == GearBox) orelse erlmachine_assembly:installed(GearBox, Release),
    {ok, Release}.

-spec switch(GearBox::assembly(), Gear::assembly(), Part::assembly()) ->
                    success(Release::assembly()) | failure(E::term(), R::term(),  Rejected::assembly()).
switch(GearBox, Gear, Part) ->
    {ok, Body} = erlmachine_transmission:switch_model(Gear, Part, body(Gear)),
    Release = body(Gear, Body),
    erlmachine_transmission:switched(GearBox, Release, Part),
    {ok, Release}.

-spec replace(GearBox::assembly(), Gear::assembly(), Repair::assembly()) ->
                     success(Release::assembly()) | failure(E::term(), R::term(), Rejected::assembly()).
replace(GearBox, Gear, Repair) ->
    {ok, Body} = erlmachine_assembly:replace_model(Gear, Repair, body(Gear)),
    Release = body(Gear, Body),
    erlmachine_assembly:replaced(GearBox, Release, Repair),
    {ok, Repair}.

%% Potentially transmit will be able to provide chained processing over list of elements;
-spec transmit(GearBox::assembly(), Gear::assembly(), Part::assembly(), Motion::term()) ->
                      success(Result::term(), Release::assembly()) | failure(E::term(), R::term(), Rejected::assembly()).
transmit(_GearBox, Gear, Part, Motion) ->
    {ok, Result, Body} = erlmachine_transmission:transmit_model(Gear, Part, Motion, body(body)),
    Release = body(Gear, Body),
    {ok, Result, Release}.

-spec call(GearBox::assembly(), Gear::assembly(), Req::term()) -> 
                  ignore.
call(_Gearbox, _Gear, _Req) -> 
    ignore.

-spec cast(GearBox::assembly(), Gear::assembly(), Message::term()) -> 
                  ignore.
cast(_Gearbox, _Gear, _Message) -> 
    ignore.

-spec accept(GearBox::assembly(), Gear::assembly(), Criteria::term()) ->
                    success(Report::term(), Release::assembly())| failure(E::term(), R::term(), Rejected::assembly()).
accept(GearBox, Gear, Criteria) ->
    {Tag, Result, Body} = erlmachine_assembly:accept_model(Gear, Criteria, body(Gear)),
    Release = body(Gear, Body),
    case Tag of 
        ok ->
            Report = Result,
            erlmachine_assembly:accepted(GearBox, Release, Criteria, Report),
            {ok, Result, Release};
        error ->
            {_, Report} = Result,
            erlmachine_assembly:rejected(GearBox, Release, Criteria, Report),
            {error, Result, Release} 
    end.

-spec rotate(GearBox::assembly(), Gear::assembly(), Part::assembly(), Motion::term()) ->
                    success(Release::assembly()) | failure(E::term(), R::term(), Rejected::assembly()).
rotate(_GearBox, Gear, Part, Motion) ->
    {ok, Body} = erlmachine_transmission:rotate_model(Gear, Part, Motion, body(body)),
    Release = body(Gear, Body),
    {ok, Release}.

-spec overload(GearBox::assembly(), Gear::assembly(), Load::term()) ->
                      success(Release::assembly()) | failure(E::term(), R::term(), Reject::assembly()).
overload(GearBox, Gear, Load) ->
    {ok, Body} = erlmachine_system:overload_model(Gear, Load, body(Gear)),
    Release = body(Gear, Body),
    erlmachine_system:overloaded(GearBox, Release, Load),
    {ok, Release}.

-spec block(GearBox::assembly(), Gear::assembly(), Part::assembly(), Failure::term()) ->
                      success(Release::assembly()) | failure(E::term(), R::term(), Reject::assembly()).
block(GearBox, Gear, Part, Failure) ->
    {ok, Body} = erlmachine_system:block_model(Gear, Part, Failure, body(Gear)),
    Release = body(Gear, Body),
    erlmachine_system:blocked(GearBox, Release, Part, Failure),
    {ok, Release}.

-spec info(GearBox::assembly(), Gear::assembly(), Message::term()) -> 
                  ignore.
info(_Gearbox, _Gear, _Message) -> 
    ignore.

-spec uninstall(GearBox::assembly(), Gear::assembly(), Reason::term()) -> 
    ok.
uninstall(GearBox, Gear, Reason) ->
    Mount = mount(Gear),
    {ok, Body} = erlmachine_assembly:uninstall_model(Gear, Reason, body(Gear)),
    Release = body(Gear, Body),
    (Mount /= undefined) andalso erlmachine_assembly:uninstalled(Mount, Reason, Release),
    (Mount == GearBox) orelse erlmachine_assembly:uninstalled(GearBox, Reason, Release),
    ok.

-spec parts(Gear::assembly()) -> list(assembly()).
parts(Gear) ->
    Product = erlmachine_assembly:product(Gear),
    Product#gear.parts.

-spec parts(Gear::assembly(), Parts::list(assembly())) -> Release::assembly().
parts(Gear, Parts) ->
    Product = erlmachine_assembly:product(Gear),
    erlmachine_assembly:product(Gear, Product#gear{parts=Parts}).

-spec body(Gear::assembly()) -> Body::term().
body(Gear) ->
    Product = erlmachine_assembly:product(Gear),
    Product#gear.body.

-spec body(Gear::assembly(), Body::term()) -> Release::assembly().
body(Gear, Body) ->
    Product = erlmachine_assembly:product(Gear),
    erlmachine_assembly:product(Gear, Product#gear{body=Body}).

-spec mount(Gear::assembly()) -> Mount::assembly().
mount(Gear) ->
    Product = erlmachine_assembly:product(Gear),
    Product#gear.mount.

-spec mount(Gear::assembly(), Mount::assembly()) -> Release::assembly().
mount(Gear, Mount) ->
    Product = erlmachine_assembly:product(Gear),
    erlmachine_assembly:product(Gear, Product#gear{mount=Mount}).

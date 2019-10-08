-module(erlmachine_gear).

-include("erlmachine_factory.hrl").
-include("erlmachine_system.hrl").

-export([
         install/2, 
         switch/3, 
         overload/3, block/4,
         replace/3,
         rotate/4, transmit/4,
         uninstall/3
        ]).

%% The main difference between gear and shaft in the next - gear as working element, shaft is transmitter instead; 
-record(gear, {body::term(), parts::assembly(), mount::assembly()}).

-export_type gear()::#gear{}.

-spec install(GearBox::assembly(), Gear::assembly()) -> 
                     success(Release::assembly()) | failure(E, R, Rejected::assembly()).
install(GearBox, Gear) ->
    Mount = mount(Gear),
    Result = {ok, Body} = erlmachine_assembly:install_model(Gear),
    %% We are going to add error handling later; 
    Release = body(Assembly, Body),
    erlmachine_assembly:installed(Mount, Release),
    (Mount == GearBox) orelse erlmachine_assembly:installed(GearBox, Release),
    {ok, Release}.

-spec switch(GearBox::assembly(), Gear::assembly(), Part::assembly()) ->
                    success(Release::assembly()) | failure(E, R,  Rejected::assembly()).
switch(GearBox, Gear, Part) ->,
    Result = {ok, Body} = erlmachine_transmission:switch_model(Gear, Part, body(Gear)),
    Release = body(Gear, Body),
    erlmachine_transmission:switched(GearBox, Release, Part),
    {ok, Release}.

-spec replace(GearBox::assembly(), Gear::assembly(), Repair::assembly()) ->
                     success(Release::assembly()) | failure(E, R, Rejected::assembly()).
replace(GearBox, Gear, Repair) ->
    Result = {ok, Body} = erlmachine_assembly:replace_model(Gear, Repair, body(Gear)),
    Release = body(Gear, Body),
    erlmachine_assembly:replaced(GearBox, Release, Repair),
    {ok, Repair}.

%% Potentially transmit will be able to provide composed processing over list of elements;
-spec transmit(GearBox::assembly(), Gear::assembly(), Part::assembly(), Motion::term()) ->
                      success(Result::term(), Release::assembly()) | failure(E, R, Rejected::assembly()).
transmit(GearBox, Gear, Part, Motion) ->
    {ok, Result, Body} = erlmachine_transmission:transmit_model(Gear, Part, Motion, body(body)),
    Release = body(Gear, Body),
    {ok, Result, Release}.

-spec call(GearBox::assembly(), Gear::assembly(), Motion::term()) -> 
                  ignore.
call(Gearbox, Gear, Req) -> 
    ignore.

-spec cast(GearBox::assembly(), Gear::assembly(), Message::term()) -> 
                  ignore.
erlmachine_gear:cast(Gearbox, Gear, Message) -> 
    ignore.

-spec accept(GearBox::assembly(), Gear::assembly(), Criteria::term()) ->
                    success(Report::term(), Release::assembly())| failure(E, R, Rejected::assembly()).
accept(Gearbox, Gear, Criteria) ->
    {Tag, Result, Body} = erlmachine_assembly:accept_model(Gear, Motion, body(Gear)),
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
rotate(GearBox, Gear, Part, Motion) ->
    {ok, Body} = erlmachine_transmission:rotate_model(Gear, Part, Motion, body(body)),
    Release = body(Gear, Body),
    {ok, Release}.

-spec overload(GearBox::assembly(), Gear::assembly(), Load::term()) ->
                      success(Release::assembly()) | failure(E, R, Reject::assembly()).
overload(GearBox, Gear, Load) ->
    Result = {ok, Body} = erlmachine_system:overload_model(Gear, Load, body(Gear)),
    Release = body(Gear, Body),
    erlmachine_system:overloaded(GearBox, Release, Load),
    {ok, Release}.

-spec block(GearBox::assembly(), Gear::assembly(), Part::assembly(), Failure::failure()) ->
                      success(Release::assembly()) | failure(E, R, Reject::assembly()).
block(GearBox, Gear, Part, Failure) ->
    Result = {ok, Body} = erlmachine_system:block_model(Gear, Part, Failure, body(Gear)),
    Release = body(Gear, Body),
    erlmachine_system:blocked(GearBox, Release, Part, Failure),
    {ok, Release}.

-spec info(GearBox::assembly(), Gear::assembly(), Message::term()) -> 
                  ignore.
erlmachine_gear:info(Gearbox, Gear, Message) -> 
    ignore.

-spec uninstall(GearBox::assembly(), Gear::assembly(), Reason::term()) -> 
    ok.
uninstall(GearBox, Gear, Reason) ->
    Result = {ok, Body} = erlmachine_assembly:uninstall_model(Gear, Reason, mount(Gear)),
    Release = body(Gear, Body),
    erlmachine_assembly:uninstalled(Mount, Reason, Release),
    (Mount == GearBox) orelse erlmachine_assembly:uninstalled(GearBox, Reason, Release),
    ok.

-spec parts(Gear::assembly()) -> list(assembly()).
parts(Gear) ->
    Product = erlmachine_assembly:product(Gear),
    [erlmachine_assembly:serial_no(Part) || Part  <- Product#gear.parts].

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

%% overloaded(Name, Assembly, Part, Load)
%% blocked(Name, Assembly, Part, Failure)
%% attached(Name, Assembly, Part, Extension)
%% detached(Name, Assembly, Part, Extension)
%% replaced(Name, Assembly, Repair)
%% switched(Name, Assembly, Part, Extension)
%% accepted(Name, Assembly, Part, Criteria)
%% rejected(Name, Assembly, Part, Criteria)

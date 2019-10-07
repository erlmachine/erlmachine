-module(erlmachine_gear).

-include("erlmachine_factory.hrl").
-include("erlmachine_system.hrl").

-export([install/2]).

-callback install(SN::serial_no(), MN::model_no(), PN::part_no(), Options::list()) -> 
    success(term()) | failure(term(), term()) | failure(term()).

-callback uninstall(SN::serial_no(), Body::term(), Reason::term()) -> 
    success(term()) | failure(term(), term()) | failure(term()).

%% The main difference between gear and shaft in the next - gear as working element, shaft is transmitter instead; 
-record(gear, {body::term(), parts::assembly(), mount::assembly()}).

-export_type gear()::#gear{}.

-spec install(GearBox::assembly(), Gear::assembly()) -> 
                     success(Release::assembly()) | failure(E, R).
install(GearBox, Gear) ->
    Mount = Gear#gear.mount,
    Result = {ok, Body} = erlmachine_assembly:install_model(Gear),
    %% We are going to add error handling later; 
    Product = erlmachine_assembly:product(Assembly),
    Release = erlmachine_assembly:product(Assembly, Product#gear{body = Body}),
    erlmachine_assembly:installed(Mount, Release),
    (Mount == GearBox) orelse erlmachine_assembly:installed(GearBox, Release),
    {ok, Release}.

-spec switch(GearBox::assembly(), Gear::assembly(), Part::assembly()) ->
                    success(Release::assembly()) | failure(E, R).
switch(GearBox, Gear, Part) ->
    Product = erlmachine_assembly:product(Gear),
    Result = {ok, Body} = erlmachine_transmission:switch_model(Gear, Part, Product#gear.body),
    Release = erlmachine_assembly:product(Gear, Product#gear{body = Body}),
    erlmachine_transmission:switched(GearBox, Release, Part),
    {ok, Release}.

-spec replace(GearBox::assembly(), Gear::assembly(), Repair::assembly()) ->
                     success(Release::assembly()) | failure(E, R).
replace(GearBox, Gear, Repair) ->
    Product = erlmachine_assembly:product(Gear),
    Result = {ok, Body} = erlmachine_assembly:replace_model(Gear, Repair, Product#gear.body),
    Release = erlmachine_assembly:product(Gear, Product#gear{body = Body}),
    erlmachine_assembly:replaced(GearBox, Release, Repair),
    {ok, Repair}.

-spec transmit(GearBox::assembly(), Gear::assembly(), Motion::term()) ->
                     success(Result::term(), Body::term()) | failure(E, R).
transmit(GearBox, Gear, Motion) ->
    Product = erlmachine_assembly:product(Gear),
    {ok, Result, Body} = erlmachine_transmission:transmit(Gear, Motion, Product#gear.body),
    Release = erlmachine_assembly:product(Gear, Product#gear{body = Body}),
    {ok, Result}.

-spec call(GearBox::assembly(), Gear::assembly(), Motion::term()) -> 
                  ignore.
call(Gearbox, Gear, Req) -> 
    ignore.

-spec cast(GearBox::assembly(), Gear::assembly(), Message::term()) -> 
                  ignore.
erlmachine_gear:cast(Gearbox, Gear, Message) -> 
    ignore.

erlmachine_gear:accept(Gearbox, Gear, Criteria),

erlmachine_gear:rotate(GearBox, Gear, Motion),

-spec overload(GearBox::assembly(), Gear::assembly(), Load::term()) ->
                      success(Release::assembly()) | failure(E, R).
overload(GearBox, Gear, Load) ->
    Product = erlmachine_assembly:product(Gear),
    Result = {ok, Body} = erlmachine_system:overload_model(Gear, Load, Product#gear.body),
    Release = erlmachine_assembly:product(Gear, Product#gear{body = Body}),
    erlmachine_system:overloaded(GearBox, Release, Load),
    {ok, Release}.

-spec block(GearBox::assembly(), Gear::assembly(), Part::assembly(), Failure::failure()) ->
                      success(Release::assembly()) | failure(E, R).
block(GearBox, Gear, Part, Failure) ->
    Product = erlmachine_assembly:product(Gear),
    Result = {ok, Body} = erlmachine_system:block_model(Gear, Part, Failure, Product#gear.body),
    Release = erlmachine_assembly:product(Gear, Product#gear{body = Body}),
    erlmachine_system:blocked(GearBox, Release, Part, Failure),
    {ok, Release}.

-spec info(GearBox::assembly(), Gear::assembly(), Message::term()) -> 
                  ignore.
erlmachine_gear:info(Gearbox, Gear, Message) -> 
    ignore.

-spec uninstall(GearBox::assembly(), Gear::assembly(), Reason::term()) -> 
    ok.
uninstall(GearBox, Gear, Reason) ->
    Mount = Gear#gear.mount,
    Product = erlmachine_assembly:product(Gear),
    Result = {ok, Body} = erlmachine_assembly:uninstall_model(Gear, Reason, Product#gear.body),
    Release = erlmachine_assembly:product(Gear, Product#gear{body = Body}),
    erlmachine_assembly:uninstalled(Mount, Reason, Release),
    (Mount == GearBox) orelse erlmachine_assembly:uninstalled(GearBox, Reason, Release),
    ok.

%% overloaded(Name, Assembly, Part, Load)
%% blocked(Name, Assembly, Part, Failure)
%% attached(Name, Assembly, Part, Extension)
%% detached(Name, Assembly, Part, Extension)
%% replaced(Name, Assembly, Repair)
%% switched(Name, Assembly, Part, Extension)
%% accepted(Name, Assembly, Part, Criteria)
%% rejected(Name, Assembly, Part, Criteria)

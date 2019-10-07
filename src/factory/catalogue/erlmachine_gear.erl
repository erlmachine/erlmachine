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
    (Mount /= GearBox) orelse erlmachine_assembly:installed(GearBox, Release),
    {ok, Release}.

-spec uninstall(GearBox::assembly(), Gear::assembly(), Reason::term()) -> 
    ok.
uninstall(GearBox, Gear, Reason) ->
    Mount = Gear#gear.mount,
    Product = erlmachine_assembly:product(Gear),
    Result = {ok, Body} = erlmachine_assembly:uninstall_model(Gear, Reason, Product#gear.body),
    Release = erlmachine_assembly:product(Gear, Product#gear{body = Body}),
    erlmachine_assembly:uninstalled(Mount, Reason,  Release),
    (Mount /= GearBox) orelse erlmachine_assembly:uninstalled(GearBox, Reason, Release),
    ok.

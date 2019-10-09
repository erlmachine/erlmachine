-module(erlmachine_gearbox).

%% The main puprouse of a product module is to provide API between clients part and system; 

%% Gearbox is a component which responsible for reliable spatial placement for all processes;
%% Gearbox is the place where shafts, gears and axles are fixed. 
%% Gearbox is the main container component
%% The gearbox is divided on so called stages (stage is a torgue between two independent gears);

-export([
         install/1,
         accept/2,
         attach/2, detach/2,
         uninstall/2
        ]).

-export([
         parts/1, parts/2,
         specs/1, specs/2,
         body/1, body/2
        ]).

-include("erlmachine_factory.hrl").
-include("erlmachine_system.hrl").


-record(gearbox, {
                  input::assembly(),
                  body::term(),
                  parts=[]::list(assembly()),
                  placement::term(),
                  %% Placement can be implemented by various ways and then represented by different formats; 
                  %% Each implementation can do that over its own discretion;
                  %% Erlmachine do that accordingly to YAML format;
                  specs=[]::list(map()),
                  output::assembly()
                 }
       ).

-type gearbox() :: #gearbox{}.

-export_type([gearbox/0]).

-spec install(GearBox::assembly()) -> 
                     success(Release::assembly()) | failure(E::term(), R::term(), Rejected::assembly()).
install(GearBox) ->
    {ok, Body} = erlmachine_assembly:install_model(GearBox),
    %% We are going to add error handling later; 
    Release = body(GearBox, Body),
    {ok, Release}.

-spec attach(GearBox::assembly(), Part::assembly()) ->
                    success(Release::assembly()) | failure(E::term(), R::term(), Rejected::assembly()).
attach(GearBox, _Part) ->
    {ok, GearBox}. %% TODO

-spec detach(GearBox::assembly(), ID::serial_no()) ->
                    success(Release::assembly()) | failure(E::term(), R::term(), Rejected::assembly()).
detach(GearBox, _ID) ->
    {ok, GearBox}. %% TODO

-spec uninstall(GearBox::assembly(), Reason::term()) -> 
                       ok.
uninstall(GearBox, Reason) ->
    {ok, _Body} = erlmachine_assembly:uninstall_model(GearBox, Reason, body(GearBox)),
    ok.

-spec accept(GearBox::assembly(), Criteria::term()) ->
                    success(Report::term(), Release::assembly())| failure(E::term(), R::term(), Rejected::assembly()).
accept(GearBox, Criteria) ->
    Result = erlmachine_assembly:accept_model(GearBox, Criteria, body(GearBox)),
    %% I guess at that place needs to be satisfied individual check over all contained in case parts; 
    Result.

-spec specs(GearBox::assembly()) -> list(assembly()).
specs(GearBox) ->
    %% Procs::list(map()),
    Product = erlmachine_assembly:product(GearBox),
    Product#gearbox.specs.

-spec specs(GearBox::assembly(), Specs::list(map())) -> Release::assembly().
specs(GearBox, Specs) ->
    Product = erlmachine_assembly:product(GearBox),
    erlmachine_assembly:product(GearBox, Product#gearbox{specs=Specs}).

-spec parts(GearBox::assembly()) -> list(assembly()).
parts(GearBox) ->
    %% Procs::list(map()),
    Product = erlmachine_assembly:product(GearBox),
    Product#gearbox.parts.

-spec parts(GearBox::assembly(), Parts::list(assembly())) -> Release::assembly().
parts(GearBox, Parts) ->
    Product = erlmachine_assembly:product(GearBox),
    erlmachine_assembly:product(GearBox, Product#gearbox{parts=Parts}).

-spec body(GearBox::assembly()) -> Body::term().
body(GearBox) ->
    Product = erlmachine_assembly:product(GearBox),
    Product#gearbox.body.

-spec body(GearBox::assembly(), Body::term()) -> Release::assembly().
body(GearBox, Body) ->
    Product = erlmachine_assembly:product(GearBox),
    erlmachine_assembly:product(GearBox, Product#gearbox{body=Body}).

%% processes need to be instantiated by builder before;

%% detached;
%% (Reason == normal) orelse erlmachine_system:crash(Assembly, Reason),

%% erlmachine_system:damage(Assembly, Failure).

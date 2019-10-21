-module(erlmachine_gearbox).

%% The main puprouse of a product module is to provide API between clients part and system; 

%% Gearbox is a component which responsible for reliable spatial placement for all processes;
%% Gearbox is the place where shafts, gears and axles are fixed. 
%% Gearbox is the main container component
%% The gearbox is divided on so called stages (stage is a torgue between two independent gears);

-export([
         install/1,
         attach/2, detach/2,
         accept/2,
         uninstall/2
        ]).

-export([
         gearbox/1, gearbox/2,
         input/1, input/2,
         body/1, body/2,
         env/1, env/2,
         output/1, output/2
        ]).

-include("erlmachine_factory.hrl").
-include("erlmachine_system.hrl").

-callback install(SN::serial_no(), Body::term(), Options::term(), Env::list()) -> 
    success(term()) | failure(term(), term(), term()) | failure(term()).

-callback uninstall(SN::serial_no(), Reason::term(), Body::term()) -> 
    success() | failure(term(), term()) | failure(term()).

-callback accept(SN::serial_no(), Criteria::term(), Body::term()) -> 
    success(term(), term()) | failure(term(), term(), term()) | failure(term()).

-callback attach(SN::serial_no(), ID::serial_no(), Body::term()) -> 
    success(term()) | failure(term(), term(), term()) | failure(term()).

-callback detach(SN::serial_no(), ID::serial_no(), Body::term()) -> 
    success(term()) | failure(term(), term(), term()) | failure(term()).

-record(gearbox, {
                  input::assembly(),
                  body::term(),
                  env::term(),
                  placement::term(),
                  %% Placement can be implemented by various ways and then represented by different formats; 
                  %% Each implementation can do that over its own discretion;
                  %% Erlmachine do that accordingly to YAML format;
                  output::assembly()
                 }
       ).

-type gearbox() :: #gearbox{}.

-export_type([gearbox/0]).

-spec gearbox(Body::term()) -> gearbox().
gearbox(Body) ->
    #gearbox{body=Body}.

-spec gearbox(Body::term(), Env::term()) -> gearbox().
gearbox(Body, Env) ->
    #gearbox{body=Body, env=Env}.

-spec install(GearBox::assembly()) -> 
                     success(Release::assembly()) | failure(E::term(), R::term(), Rejected::assembly()).
install(GearBox) ->
    ModelName = erlmachine_assembly:model_name(GearBox),
    SN = erlmachine_assembly:serial_no(GearBox),
    Env = erlmachine_gearbox:env(GearBox), 
    Options = erlmachine_assembly:model_options(GearBox),
    {ok, Body} = ModelName:install(SN, body(GearBox), Options, Env),
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
    ModelName = erlmachine_assembly:model_name(GearBox),
    SN = erlmachine_assembly:serial_no(GearBox),
    ok = ModelName:uninstall(SN, Reason, body(GearBox)),
    ok.

-spec accept(GearBox::assembly(), Criteria::term()) ->
                    success(Report::term(), Release::assembly())| failure(E::term(), R::term(), Rejected::assembly()).
accept(GearBox, Criteria) ->
    ModelName = erlmachine_assembly:model_name(GearBox),
    SN = erlmachine_assembly:serial_no(GearBox),
    {Tag, Result, Body} = ModelName:accept(SN, Criteria, body(GearBox)),
    Release = body(GearBox, Body),
    case Tag of 
        ok ->
            Report = Result,
            {ok, Result, Release};
        error ->
            {_, Report} = Result,
            {error, Result, Release} 
    end.

-spec body(GearBox::assembly()) -> Body::term().
body(GearBox) ->
    Product = erlmachine_assembly:product(GearBox),
    Product#gearbox.body.

-spec body(GearBox::assembly(), Body::term()) -> Release::assembly().
body(GearBox, Body) ->
    Product = erlmachine_assembly:product(GearBox),
    erlmachine_assembly:product(GearBox, Product#gearbox{body=Body}).

-spec input(GearBox::assembly()) -> assembly().
input(GearBox) ->
    GearBox#gearbox.input.

-spec input(GearBox::assembly(), Input::assembly()) -> Release::assembly().
input(GearBox, Input) ->
    Product = erlmachine_assembly:product(GearBox),
    erlmachine_assembly:product(GearBox, Product#gearbox{input=Input}).

-spec output(GearBox::assembly()) -> assembly().
output(GearBox) ->
    GearBox#gearbox.output.

-spec output(GearBox::assembly(), Output::assembly()) -> Release::assembly().
output(GearBox, Output) ->
    Product = erlmachine_assembly:product(GearBox),
    erlmachine_assembly:product(GearBox, Product#gearbox{output=Output}).

-spec env(GearBox::assembly()) -> term().
env(GearBox) ->
    GearBox#gearbox.env.

-spec env(GearBox::assembly(), Env::term()) -> Release::assembly().
env(GearBox, Env) ->
    Product = erlmachine_assembly:product(GearBox),
    erlmachine_assembly:product(GearBox, Product#gearbox{env=Env}).

%% processes need to be instantiated by builder before;

%% detached;
%% (Reason == normal) orelse erlmachine_system:crash(Assembly, Reason),

%% erlmachine_system:damage(Assembly, Failure).

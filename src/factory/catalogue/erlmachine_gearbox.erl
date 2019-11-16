-module(erlmachine_gearbox).

%% The main puprouse of a product module is to provide API between clients part and system; 

%% Gearbox is a component which is responsible for reliable spatial placement for all processes (parts);
%% Gearbox is the place where shafts, gears and axles are located. 
%% Gearbox is the main container within all system topology;
%% The gearbox is divided on so called stages (stage is a torgue between two independent gears);
%% Input is the special place which is recommended by design for to all input commands;
%% Output is considered like a special place where measurements and monitoring abilitites need to be provided;
%% The most convinient way to implement input and output like a shafts;
%% This agreement allows to us attach gearboxes together and with other parts by attach call;
%% This can evolve messaging systems reusage;

-export([
         install/1,
         rotate/2,
         transmit/2, transmit/3,
         mount/2, unmount/2,
         accept/2,
         attach/2, detach/2,
         uninstall/2
        ]).

-export([
         gearbox/1, gearbox/2,
         input/1, input/2,
         body/1, body/2,
         env/1, env/2,
         output/1, output/2
        ]).

-export([specs/1, spec/2]).

-include("erlmachine_factory.hrl").
-include("erlmachine_system.hrl").


-callback install(SN::serial_no(), IDs::list(serial_no()), Body::term(), Options::term(), Env::list()) -> 
    success(term()) | failure(term(), term(), term()) | failure(term()).

-callback uninstall(SN::serial_no(), Reason::term(), Body::term()) -> 
    success() | failure(term(), term()) | failure(term()).

-callback accept(SN::serial_no(), Criteria::term(), Body::term()) -> 
    success(term(), term()) | failure(term(), term(), term()) | failure(term()).

-callback mount(SN::serial_no(), ID::serial_no(), Body::term()) -> 
    success(term()) | failure(term(), term(), term()) | failure(term()).

-callback unmount(SN::serial_no(), ID::serial_no(), Body::term()) -> 
    success(term()) | failure(term(), term(), term()) | failure(term()).

-record(gearbox, {
                  input::assembly(),
                  body::term(),
                  %% Body can be implemented by various ways and then be represented by different formats; 
                  %% Each implementation can do that over its own discretion;
                  %% Erlmachine do that accordingly to YAML format;
                  env::term(),
                  output::assembly()
                 }
       ).

%% I am thinking about graph ipmlementation of body;
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
    IDs = [erlmachine_assembly:serial_no(Part)|| Part <- erlmachine_assembly:parts(GearBox)], 
    {ok, Body} = ModelName:install(SN, IDs, body(GearBox), Options, Env),
    %% We are going to add error handling later; 
    Release = body(GearBox, Body),
    {ok, Release}.

-spec mount(GearBox::assembly(), Part::assembly()) ->
                    success(Release::assembly()) | failure(E::term(), R::term(), Rejected::assembly()).
mount(GearBox, Part) ->
    ModelName = erlmachine_assembly:model_name(GearBox),
    SN = erlmachine_assembly:serial_no(GearBox), ID = erlmachine_assembly:serial_no(Part),
    {ok, Body} = ModelName:mount(SN, ID, body(GearBox)),
    Release = erlmachine_assembly:attach(body(GearBox, Body), Part),
    %% At that place we don't issue any events (cause is gearbox issue level);
    {ok, Release}. %% TODO

-spec unmount(GearBox::assembly(), ID::serial_no()) ->
                    success(Release::assembly()) | failure(E::term(), R::term(), Rejected::assembly()).
unmount(GearBox, ID) ->
    ModelName = erlmachine_assembly:model_name(GearBox),
    SN = erlmachine_assembly:serial_no(GearBox),
    {ok, Body} = ModelName:unmount(SN, ID, body(GearBox)),
    Release = erlmachine_assembly:detach(body(GearBox, Body), ID),
    {ok, Release}. %% TODO

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
            {ok, Report, Release};
        error ->
            {_, Report} = Result,
            {error, Report, Release} 
    end.

-spec rotate(GearBox::assembly(), Motion::term()) ->
                    Motion::term().
rotate(GearBox, Motion) ->
    Input = input(GearBox),
    erlmachine_transmission:rotate(Input, Motion).

-spec transmit(GearBox::assembly(), Motion::term()) ->
                      success(term()) | failure(term(), term()).
transmit(GearBox, Motion) ->
    Input = input(GearBox),
    erlmachine_transmission:transmit(Input, Motion).

-spec transmit(GearBox::assembly(), Motion::term(), TimeOut::integer()) ->
                      success(term()) | failure(term(), term()).
transmit(GearBox, Motion, TimeOut) ->
    Input = input(GearBox),
    erlmachine_transmission:transmit(Input, Motion, TimeOut).

-spec attach(GearBox::assembly(), Part::assembly()) ->
                    success(term()) | failure(term(), term()).
attach(GearBox, Part) ->
    Output = output(GearBox),
    erlmachine_transmission:attach(Output, Part).

-spec detach(GearBox::assembly(), ID::serial_no()) -> 
                    success(term()) | failure(term(), term()).
detach(GearBox, ID) ->
    Output = output(GearBox),
    erlmachine_transmission:detach(Output, ID).

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
    Product = erlmachine_assembly:product(GearBox),
    Env = Product#gearbox.env,
    Env.

-spec env(GearBox::assembly(), Env::term()) -> Release::assembly().
env(GearBox, Env) ->
    Product = erlmachine_assembly:product(GearBox),
    erlmachine_assembly:product(GearBox, Product#gearbox{env=Env}).

-spec spec(GearBox::assembly(), Part::assembly()) -> map().
spec(GearBox, Part) ->
    Spec = erlmachine_assembly:spec(GearBox, erlmachine_assembly:mounted(Part, GearBox)),
    Spec.

-spec specs(GearBox::assembly()) -> list(map()).
specs(GearBox) ->
    Parts = erlmachine_assembly:parts(GearBox),
    Specs = [spec(GearBox, Part)|| Part <- Parts],
    Specs.

%% processes need to be instantiated by builder before;

%% detached;
%% (Reason == normal) orelse erlmachine_system:crash(Assembly, Reason),

%% erlmachine_system:damage(Assembly, Failure).

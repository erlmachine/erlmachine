-module(erlmachine_gearbox).

%% The main puprouse of a product module is to provide API between client's part and system; 

%% Gearbox is a component which is responsible for reliable spatial placement for all processes (parts);
%% Gearbox is the place where shafts, gears and axles are located. 
%% Gearbox is the main container within all system topology;
%% The gearbox is divided on so called stages (stage is a torgue between two independent gears);
%% Input is the special place which is recommended by design for all input commands;
%% Output is considered like a special place where measurements and monitoring abilitites need to be provided;
%% The most convinient way to implement input and output like a shafts;
%% This agreement allows to us attach gearboxes together and with other parts by attach call;
%% This can evolve messaging systems reusage;

-export([rotate/2]).
-export([transmit/2]).
-export([connect/2, disconnect/2]).

-export([
         install/1,
         schema/2,
         attach/3, detach/2,
         accept/2,
         uninstall/2
        ]).

-export([gearbox/1]).

-export([
         input/1, input/2,
         env/1, env/2,
         output/1, output/2
        ]).

-export([master/1]).

-export([mounted/2]).

-export([parts/2, parts/3, parts/4]).

-export([find/2]).

-export([record_name/0, attributes/0]).

-include("erlmachine_factory.hrl").
-include("erlmachine_system.hrl").


-callback install(SN::serial_no(), IDs::list(serial_no()), Body::term(), Opt::term(), Env::list()) -> 
    success(term()) | failure(term(), term(), term()) | failure(term()).

-callback uninstall(SN::serial_no(), Reason::term(), Body::term()) -> 
    success() | failure(term(), term(), term()).

-callback accept(SN::serial_no(), Criteria::criteria(), Body::term()) -> 
    success(term(), term()) | failure(term(), term(), term()).

-callback attach(SN::serial_no(), Register::term(), ID::serial_no(), Body::term()) -> 
    success(term()) | failure(term(), term(), term()) | failure(term()).

-callback detach(SN::serial_no(), ID::serial_no(), Body::term()) -> 
    success(term()) | failure(term(), term(), term()) | failure(term()).

-callback schema(SN::serial_no(), Schema::term(), Body::term()) ->
    success(term(), term()) | failure(term(), term(), term()) | failure(term()).

-optional_callbacks([schema/3]).

-record(gearbox, {
                  input::serial_no(),
                  %% Schema can be implemented by various ways and then be represented by different formats; 
                  %% Each implementation can do that over its own discretion;
                  %% Erlmachine do that accordingly to YAML format;
                  env::term(),
                  output::serial_no()
                 }
       ).

-type gearbox() :: #gearbox{}.

-export_type([gearbox/0]).

-spec record_name() -> atom().
record_name() ->
    gearbox.

-spec attributes() -> list(atom()).
attributes() ->
    record_info(fields, gearbox).

-spec gearbox(Env::term()) -> gearbox().
gearbox(Env) ->
    #gearbox{ env=Env }.

-spec master(GearBox::assembly()) -> Release::assembly().
master(GearBox) ->
    Schema = erlmachine_assembly:schema(),
    erlmachine_assembly:schema(GearBox, Schema).

-spec mounted(GearBox::assembly(), Parts::list(assembly())) -> Release::assembly().
mounted(GearBox, Parts) ->
    Mounted = [erlmachine_assembly:mounted(Part, GearBox)|| Part <- Parts],
    Release = erlmachine_assembly:parts(GearBox, Mounted),
    Release.

%% We are going to provide access by path gearbox.shaft.# (like rabbitmq notation) too;

-spec find(GearBox::assembly(), SN::serial_no()) -> 
                  assembly() | false.
find(GearBox, SN) ->
    Schema = erlmachine_assembly:schema(GearBox),
    erlmachine_schema:vertex(Schema, SN).

-spec install(GearBox::assembly()) -> 
                     success(Release::assembly()) | failure(E::term(), R::term(), Rejected::assembly()).
install(GearBox) ->
    ModelName = erlmachine_assembly:model_name(GearBox),
    SN = erlmachine_assembly:serial_no(GearBox),
    Env = erlmachine_gearbox:env(GearBox), 
    Options = erlmachine_assembly:model_options(GearBox),
    Parts = erlmachine_assembly:parts(GearBox),
    IDs = [erlmachine_assembly:serial_no(Part)|| Part <- Parts],
    {ok, State} = ModelName:install(SN, IDs, state(GearBox), Options, Env),
    %% We are going to add error handling later;
    Release = state(GearBox, State),
    {ok, Release}.

-spec attach(GearBox::assembly(), Register::term(), Extension::assembly()) ->
                    success(Release::assembly()) | failure(E::term(), R::term(), Rejected::assembly()).
attach(GearBox, Register, Extension) ->
    ModelName = erlmachine_assembly:model_name(GearBox),
    SN = erlmachine_assembly:serial_no(GearBox), ID = erlmachine_assembly:serial_no(Extension),
 
    {ok, State} = ModelName:attach(SN, Register, ID, state(GearBox)),
    
    Part = erlmachine_assembly:mounted(Extension, GearBox),
    Release = erlmachine_assembly:add_part(state(GearBox, State), Part),
    %% At that place we don't issue any events (cause is gearbox issue level);
    {ok, Part, Release}. %% TODO

-spec detach(GearBox::assembly(), ID::serial_no()) ->
                    success(Release::assembly()) | failure(E::term(), R::term(), Rejected::assembly()).
detach(GearBox, ID) ->
    ModelName = erlmachine_assembly:model_name(GearBox),
    SN = erlmachine_assembly:serial_no(GearBox),
    {ok, State} = ModelName:detach(SN, ID, state(GearBox)),
    Release = erlmachine_assembly:remove_part(state(GearBox, State), ID),
    {ok, Release}. %% TODO

-spec accept(GearBox::assembly(), Criteria::term()) ->
                    success()| failure(E::term(), R::term(), S::term()).
accept(GearBox, Criteria) ->
    ModelName = erlmachine_assembly:model_name(GearBox),
    SN = erlmachine_assembly:serial_no(GearBox),
    {ok, Result, State} = ModelName:accept(SN, Criteria, state(GearBox)),
    Release = state(GearBox, State),
    {ok, Result, Release}.

-spec uninstall(GearBox::assembly(), Reason::term()) -> 
                       ok.
uninstall(GearBox, Reason) ->
    ModelName = erlmachine_assembly:model_name(GearBox),
    SN = erlmachine_assembly:serial_no(GearBox),
    ModelName:uninstall(SN, Reason, state(GearBox)).

-spec schema(GearBox::assembly(), Format::term()) ->
                    success(term(), term()) | failure(term(), term(), term()).
schema(GearBox, Format) ->
    ModelName = erlmachine_assembly:model_name(GearBox),
    SN = erlmachine_assembly:serial_no(GearBox),
    {ok, _, _} = ModelName:schema(SN, Format, state(GearBox)).

-spec rotate(GearBox::assembly(), Motion::term()) ->
                    Motion::term().
rotate(GearBox, Motion) ->
    Input = input(GearBox), Assembly = find(GearBox, Input),
    erlmachine_transmission:rotate(GearBox, Assembly, Motion).

-spec transmit(GearBox::assembly(), Motion::term()) ->
                      success(term()) | failure(term(), term()).
transmit(GearBox, Motion) ->
    Input = input(GearBox), Assembly = find(GearBox, Input),
    erlmachine_transmission:transmit(GearBox, Assembly, Motion).

-spec connect(GearBox::assembly(), Part::assembly()) ->
                    success(term()) | failure(term(), term()).
connect(GearBox, Part) ->
    Output = output(GearBox), Assembly = find(GearBox, Output),
    erlmachine_transmission:attach(Assembly, Part).

-spec disconnect(GearBox::assembly(), ID::serial_no()) -> 
                    success(term()) | failure(term(), term()).
disconnect(GearBox, ID) ->
    Output = output(GearBox), Assembly = find(GearBox, Output),
    erlmachine_transmission:detach(Assembly, ID).

-spec state(Axle::assembly()) -> term().
state(Axle) ->
    erlmachine_assembly:schema(Axle).

-spec state(Axle::assembly(), Schema::term()) -> assembly().
state(Axle, Schema) ->
    erlmachine_assembly:schema(Axle, Schema).

-spec input(GearBox::assembly()) -> assembly().
input(GearBox) ->
    Product = erlmachine_assembly:product(GearBox),
    Product#gearbox.input.

-spec input(GearBox::assembly(), SN::serial_no()) -> Release::assembly().
input(GearBox, SN) ->
    Product = erlmachine_assembly:product(GearBox),
    erlmachine_assembly:product(GearBox, Product#gearbox{ input=SN }).

-spec output(GearBox::assembly()) -> assembly().
output(GearBox) ->
    GearBox#gearbox.output.

-spec output(GearBox::assembly(), SN::assembly()) -> Release::assembly().
output(GearBox, SN) ->
    Product = erlmachine_assembly:product(GearBox),
    erlmachine_assembly:product(GearBox, Product#gearbox{ output=SN }).

-spec env(GearBox::assembly()) -> term().
env(GearBox) ->
    Product = erlmachine_assembly:product(GearBox),
    Env = Product#gearbox.env,
    Env.

-spec env(GearBox::assembly(), Env::term()) -> Release::assembly().
env(GearBox, Env) ->
    Product = erlmachine_assembly:product(GearBox),
    erlmachine_assembly:product(GearBox, Product#gearbox{ env=Env }).

-spec parts(GearBox::assembly(), Parts::list(assembly())) -> Release::assembly().
parts(GearBox, Parts) ->
    Mounted = [erlmachine_assembly:mounted(Part, GearBox)|| Part <- Parts],
    erlmachine_assembly:parts(GearBox, Mounted).

-spec parts(GearBox::assembly(), Input::assembly(), Parts::list(assembly())) ->
                   Release::assembly().
parts(GearBox, Input, Parts) ->
    input(parts(GearBox, Parts), Input).

-spec parts(GearBox::assembly(), Input::serial_no(), Parts::list(assembly()), Output::serial_no()) ->
                   Release::assembly().
parts(GearBox, Input, Parts, Output) ->
    output(input(parts(GearBox, Parts), Input), Output).

%% processes need to be instantiated by builder before;

%% detached;
%% (Reason == normal) orelse erlmachine_system:crash(Assembly, Reason),

%% erlmachine_system:damage(Assembly, Failure).

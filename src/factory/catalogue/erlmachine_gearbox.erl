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
         attach/3, detach/2,
         accept/2,
         uninstall/2
        ]).

-export([form/1, submit/2]).

-export([gearbox/1]).

-export([
         input/1, input/2,
         env/1, env/2,
         output/1, output/2
        ]).

-export([master/1]).

-export([mounted/2]).

-export([parts/2, parts/3, parts/4]).

-export([find/1, find/2]).

-export([record_name/0, attributes/0]).

-include("erlmachine_factory.hrl").
-include("erlmachine_system.hrl").


-callback install(SN::serial_no(), IDs::list(term()), Schema::term(), Opt::term(), Env::list()) -> 
    success(term()) | failure(term(), term(), term()) | failure(term()).

-callback uninstall(SN::serial_no(), Reason::term(), Schema::term()) -> 
    success() | failure(term(), term(), term()).

-callback accept(SN::serial_no(), Criteria::criteria(), Schema::term()) -> 
    success(term(), term()) | failure(term(), term(), term()).

-callback attach(SN::serial_no(), Reg::term(), ID::term(), Schema::term()) -> 
    success(term()) | failure(term(), term(), term()) | failure(term()).

-callback detach(SN::serial_no(), ID::term(), Schema::term()) -> 
    success(term()) | failure(term(), term(), term()) | failure(term()).

-callback form(SN::serial_no(), Schema::term()) ->
    success(term(), term()) | failure(term(), term(), term()) | failure(term()).

-callback submit(SN::serial_no(), Form::term(), Schema::term()) ->
    success(term()) | failure(term(), term(), term()) | failure(term()).

-optional_callbacks([form/2, submit/3]).

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

-spec find(GearBox::assembly(), Label::term()) -> 
                  assembly() | false.
find(GearBox, Label) ->
    Schema = erlmachine_assembly:schema(GearBox),
    erlmachine_schema:vertex(Schema, Label).

-spec find(GearBox::assembly()) -> list().
find(GearBox) ->
    Label = erlmachine_assembly:label(GearBox),
    Schema = erlmachine_assembly:schema(GearBox),
    [erlmachine_schema:vertex(Schema, V)|| V <- erlmachine_schema:vertices(Schema), V /= Label].

-spec install(GearBox::assembly()) -> 
                     success(assembly()) | failure(term(), term(), assembly()).
install(GearBox) ->
    SN = erlmachine_assembly:serial_no(GearBox),

    Env = erlmachine_gearbox:env(GearBox), 
    Opt = erlmachine_assembly:model_options(GearBox),

    Parts = erlmachine_assembly:parts(GearBox),
    IDs = [erlmachine_assembly:label(Part)|| Part <- Parts],
    ModelName = erlmachine_assembly:model_name(GearBox),
    {ok, State} = ModelName:install(SN, IDs, state(GearBox), Opt, Env),
    %% We are going to add error handling later;
    Rel = state(GearBox, State),
    {ok, Rel}.

-spec attach(GearBox::assembly(), Reg::term(), Ext::assembly()) ->
                    success(assembly()) | failure(term(), term(), assembly()).
attach(GearBox, Reg, Ext) ->
    ModelName = erlmachine_assembly:model_name(GearBox),
    SN = erlmachine_assembly:serial_no(GearBox), ID = erlmachine_assembly:label(Ext),
 
    {ok, State} = ModelName:attach(SN, Reg, ID, state(GearBox)),
    
    Part = erlmachine_assembly:mounted(Ext, GearBox),
    Rel = erlmachine_assembly:add(state(GearBox, State), Part),
    %% At that place we don't issue any events (cause is gearbox issue level);
    {ok, Part, Rel}. %% TODO

-spec detach(GearBox::assembly(), ID::term()) ->
                    success(assembly()) | failure(term(), term(), assembly()).
detach(GearBox, ID) ->
    SN = erlmachine_assembly:serial_no(GearBox),
    ModelName = erlmachine_assembly:model_name(GearBox),

    {ok, State} = ModelName:detach(SN, ID, state(GearBox)),

    Rel = erlmachine_assembly:remove(state(GearBox, State), ID),
    {ok, Rel}. %% TODO

-spec accept(GearBox::assembly(), Criteria::term()) ->
                    success()| failure(term(), term(), term()).
accept(GearBox, Criteria) ->
    SN = erlmachine_assembly:serial_no(GearBox),
    ModelName = erlmachine_assembly:model_name(GearBox),

    {ok, Res, State} = ModelName:accept(SN, Criteria, state(GearBox)),
    Rel = state(GearBox, State),
    {ok, Res, Rel}.

-spec uninstall(GearBox::assembly(), Reason::term()) -> 
                       ok.
uninstall(GearBox, Reason) ->
    SN = erlmachine_assembly:serial_no(GearBox),
    ModelName = erlmachine_assembly:model_name(GearBox),

    ModelName:uninstall(SN, Reason, state(GearBox)).

-spec form(GearBox::assembly()) ->
                  success(term(), assembly()) | failure(term(), term(), term()).
form(GearBox) ->
    SN = erlmachine_assembly:serial_no(GearBox),
    ModelName = erlmachine_assembly:model_name(GearBox),

    Mod = ModelName, Fun = form, Args = [SN, state(GearBox)],
    Def = erlmachine:success([], state(GearBox)),

    {ok, Form, State} = erlmachine:optional_callback(Mod, Fun, Args, Def),
    {ok, Form, state(GearBox, State)}.

-spec submit(GearBox::assembly(), Form::term()) ->
                    success(term(), assembly()) | failure(term(), term(), term()).
submit(GearBox, Form) ->
    ModelName = erlmachine_assembly:model_name(GearBox),
    SN = erlmachine_assembly:serial_no(GearBox),

    {ok, Res, State} = ModelName:submit(SN, Form, state(GearBox)),
    {ok, Res, state(GearBox, State)}.

-spec rotate(GearBox::assembly(), Motion::term()) ->
                    Motion::term().
rotate(GearBox, Motion) ->
    erlmachine_transmission:rotation(GearBox, input(GearBox), Motion).

-spec transmit(GearBox::assembly(), Motion::term()) ->
                      success(term()) | failure(term(), term()).
transmit(GearBox, Motion) ->
    erlmachine_transmission:transmission(GearBox, input(GearBox), Motion).

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

-spec input(GearBox::assembly(), Part::assembly()) -> assembly().
input(GearBox, Part) ->
    Product = erlmachine_assembly:product(GearBox),
    erlmachine_assembly:product(GearBox, Product#gearbox{ input=Part }).

-spec output(GearBox::assembly()) -> assembly().
output(GearBox) ->
    GearBox#gearbox.output.

-spec output(GearBox::assembly(), Part::assembly()) -> assembly().
output(GearBox, Part) ->
    Product = erlmachine_assembly:product(GearBox),
    erlmachine_assembly:product(GearBox, Product#gearbox{ output=Part }).

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

-spec parts(GearBox::assembly(), Input::assembly(), Parts::list(assembly()), Output::assembly()) ->
                   assembly().
parts(GearBox, Input, Parts, Output) ->
    output(input(parts(GearBox, Parts), Input), Output).

%% processes need to be instantiated by builder before;

%% detached;
%% (Reason == normal) orelse erlmachine_system:crash(Assembly, Reason),

%% erlmachine_system:damage(Assembly, Failure).

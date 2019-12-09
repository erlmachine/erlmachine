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

-export([load/2]).
-export([drive/3]).
-export([connect/2, disconnect/2]).

-export([
         install/1,
         attach/3, detach/2,
         accept/2,
         uninstall/2
        ]).

-export([
         gearbox/1, gearbox/2, gearbox/3,
         input/1, input/2,
         body/1, body/2,
         env/1, env/2,
         schema/1, schema/2,
         output/1, output/2
        ]).

-export([mounted/2]).

-export([parts/2]).

-export([find/2]).

-include("erlmachine_factory.hrl").
-include("erlmachine_system.hrl").


-callback install(SN::serial_no(), IDs::list(serial_no()), Body::term(), Options::term(), Env::list()) -> 
    success(term()) | failure(term(), term(), term()) | failure(term()).

-callback uninstall(SN::serial_no(), Reason::term(), Body::term()) -> 
    success() | failure(term(), term()) | failure(term()).

-callback accept(SN::serial_no(), Criteria::term(), Body::term()) -> 
    success(term(), term()) | failure(term(), term(), term()) | failure(term()).

-callback attach(SN::serial_no(), Register::term(), ID::serial_no(), Body::term()) -> 
    success(term()) | failure(term(), term(), term()) | failure(term()).

-callback detach(SN::serial_no(), ID::serial_no(), Body::term()) -> 
    success(term()) | failure(term(), term(), term()) | failure(term()).

-record(gearbox, {
                  input::serial_no(),
                  body::term(),
                  schema::term(),
                  %% Body can be implemented by various ways and then be represented by different formats; 
                  %% Each implementation can do that over its own discretion;
                  %% Erlmachine do that accordingly to YAML format;
                  env::term(),
                  output::serial_no()
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

-spec gearbox(Body::term(), Env::term(), Schema::term()) -> gearbox().
gearbox(Body, Env, Schema) ->
    #gearbox{body=Body, env=Env, schema=Schema}.

-spec mounted(GearBox::assembly(), Parts::list(assembly())) -> Release::assembly().
mounted(GearBox, Parts) ->
    Mounted = [erlmachine_assembly:mounted(Part, GearBox)|| Part <- Parts],
    Release = erlmachine_assembly:parts(GearBox, Mounted),
    Release.

%% We are going to provide access by path gearbox.shaft.# (like rabbitmq notation) too;

-spec find(GearBox::assembly(), SN::serial_no()) -> 
                  assembly() | false.
find(GearBox, SN) ->
    Schema = schema(GearBox),
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
    {ok, Body} = ModelName:install(SN, IDs, body(GearBox), Options, Env),
    %% We are going to add error handling later;
    Release = body(GearBox, Body),
    {ok, Release}.

-spec attach(GearBox::assembly(), Register::term(), Extension::assembly()) ->
                    success(Release::assembly()) | failure(E::term(), R::term(), Rejected::assembly()).
attach(GearBox, Register, Extension) ->
    ModelName = erlmachine_assembly:model_name(GearBox),
    SN = erlmachine_assembly:serial_no(GearBox), ID = erlmachine_assembly:serial_no(Extension),
 
    {ok, Body} = ModelName:attach(SN, Register, ID, body(GearBox)),
    
    Part = erlmachine_assembly:mounted(Extension, GearBox),
    Release = erlmachine_assembly:add_part(body(GearBox, Body), Part),
    %% At that place we don't issue any events (cause is gearbox issue level);
    {ok, Part, Release}. %% TODO

-spec detach(GearBox::assembly(), ID::serial_no()) ->
                    success(Release::assembly()) | failure(E::term(), R::term(), Rejected::assembly()).
detach(GearBox, ID) ->
    ModelName = erlmachine_assembly:model_name(GearBox),
    SN = erlmachine_assembly:serial_no(GearBox),
    {ok, Body} = ModelName:detach(SN, ID, body(GearBox)),
    Release = erlmachine_assembly:remove_part(body(GearBox, Body), ID),
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

-spec load(GearBox::assembly(), Motion::term()) ->
                    Motion::term().
load(GearBox, Motion) ->
    Input = input(GearBox), Assembly = find(GearBox, Input),
    erlmachine_transmission:rotate(Assembly, Motion).

-spec drive(GearBox::assembly(), Motion::term(), TimeOut::integer()) ->
                      success(term()) | failure(term(), term()).
drive(GearBox, Motion, TimeOut) ->
    Input = input(GearBox), Assembly = find(GearBox, Input),
    erlmachine_transmission:transmit(Assembly, Motion, TimeOut).

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

-spec input(GearBox::assembly(), SN::serial_no()) -> Release::assembly().
input(GearBox, SN) ->
    Product = erlmachine_assembly:product(GearBox),
    erlmachine_assembly:product(GearBox, Product#gearbox{input=SN}).

-spec schema(GearBox::assembly()) -> term().
schema(GearBox) ->
    Product = erlmachine_assembly:product(GearBox),
    Schema = Product#gearbox.schema,
    Schema.

-spec schema(GearBox::assembly(), Schema::term()) -> Release::assembly().
schema(GearBox, Schema) ->
    Product = erlmachine_assembly:product(GearBox),
    erlmachine_assembly:product(GearBox, Product#gearbox{schema=Schema}).

-spec output(GearBox::assembly()) -> assembly().
output(GearBox) ->
    GearBox#gearbox.output.

-spec output(GearBox::assembly(), SN::assembly()) -> Release::assembly().
output(GearBox, SN) ->
    Product = erlmachine_assembly:product(GearBox),
    erlmachine_assembly:product(GearBox, Product#gearbox{output=SN}).

-spec env(GearBox::assembly()) -> term().
env(GearBox) ->
    Product = erlmachine_assembly:product(GearBox),
    Env = Product#gearbox.env,
    Env.

-spec env(GearBox::assembly(), Env::term()) -> Release::assembly().
env(GearBox, Env) ->
    Product = erlmachine_assembly:product(GearBox),
    erlmachine_assembly:product(GearBox, Product#gearbox{env=Env}).

-spec parts(GearBox::assembly(), Parts::list(assembly())) -> Release::assembly().
parts(GearBox, Parts) ->
    Mounted = [erlmachine_assembly:mounted(Part, GearBox)|| Part <- Parts],
    Release = erlmachine_assembly:parts(GearBox, Mounted),
    Release.

%% processes need to be instantiated by builder before;

%% detached;
%% (Reason == normal) orelse erlmachine_system:crash(Assembly, Reason),

%% erlmachine_system:damage(Assembly, Failure).

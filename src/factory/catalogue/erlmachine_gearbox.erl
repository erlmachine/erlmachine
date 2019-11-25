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
         gearbox/1, gearbox/2, gearbox/3,
         input/1, input/2,
         body/1, body/2,
         env/1, env/2,
         schema/1, schema/2,
         output/1, output/2
        ]).

-export([map/1, map_add/3, map_add/4, map_remove/2, map_update/2]).

-export([specs/1, spec/2]).

-export([find/2]).

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
                  schema::term(),
                  %% Body can be implemented by various ways and then be represented by different formats; 
                  %% Each implementation can do that over its own discretion;
                  %% Erlmachine do that accordingly to YAML format;
                  env::term(),
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

-spec gearbox(Body::term(), Env::term(), Schema::term()) -> gearbox().
gearbox(Body, Env, Schema) ->
    #gearbox{body=Body, env=Env, schema=Schema}.

%% We need to consider mounted field like indicator for of building mount topology; 
-spec map(GearBox::assembly()) -> ok.
map(GearBox) ->
    Schema = schema(GearBox),
    SN = erlmachine_assembly:serial_no(GearBox),
    digraph:add_vertex(Schema, SN, GearBox),
    map_schema(Schema, SN, erlmachine_assembly:parts(GearBox)), 
    ok.

-spec map_schema(Schema::term(), Vertex::serial_no(), Parts::list(assembly())) -> ok.
map_schema(_Schema, _Vertex, []) ->
    ok;
map_schema(Schema, Vertex, [Part|T]) ->
    SN = erlmachine_assembly:serial_no(Part),
    digraph:add_vertex(Schema, SN, Part),
    map_schema(Schema, SN, erlmachine_assembly:parts(Part)),
    %% TODO At this place we can represent kind of linking (mount/drive);
    Label = [],
    digraph:add_edge(Schema, Vertex, SN, Label),
    map_schema(Schema, Vertex, T).

-spec map_add(GearBox::assembly(), Assembly::assembly(), Part::assembly(), Label::term()) -> ok.
map_add(GearBox, Assembly, Part, Label) ->
    Schema = schema(GearBox),
    map_schema_add(Schema, Assembly, Part, Label),
    ok.

-spec map_add(GearBox::assembly(), Part::assembly(), Label::term()) -> ok.
map_add(GearBox, Part, Label) ->
    Schema = schema(GearBox),
    map_schema_add(Schema, GearBox, Part, Label),
    ok.

-spec map_schema_add(Schema::assembly(), Assembly::assembly(), Part::assembly(), Label::term()) -> ok.
map_schema_add(Schema, Assembly, Part, Label) ->
    SN = erlmachine_assembly:serial_no(Part),
    digraph:add_vertex(Schema, SN, Part),
    digraph:add_edge(Schema, erlmachine_assembly:serial_no(Assembly), SN, Label).

-spec map_remove(GearBox::assembly(), ID::serial_no()) -> ok.
map_remove(GearBox, ID) ->
    Schema = schema(GearBox),
    map_schema_remove(Schema, ID),
    ok.

-spec map_schema_remove(GearBox::assembly(), Assembly::assembly(), ID::serial_no()) -> ok.
map_schema_remove(Schema, ID) ->
    digraph:del_vertex(Schema, ID).

-spec map_update(GearBox::assembly(), Assembly::assembly()) -> ok.
map_update(GearBox, Assembly) ->
    Schema = schema(GearBox),
    digraph:add_vertex(Schema, erlmachine_assembly:serial_no(Assembly), Assembly),
    ok.
%% We are going to provide access by path gearbox.shaft.# (like rabbitmq notation) too;

-spec find(GearBox::assembly(), SN::serial_no()) -> 
                  assembly() | false.
find(GearBox, SN) ->
    Schema = schema(GearBox),
    case digraph:vertex(Schema, SN) of 
        {_V, Part} ->
            Part;
        _ ->
            false
    end.

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
    Input = input(GearBox), Assembly = find(GearBox, Input),
    erlmachine_transmission:rotate(Assembly, Motion).

-spec transmit(GearBox::assembly(), Motion::term()) ->
                      success(term()) | failure(term(), term()).
transmit(GearBox, Motion) ->
    Input = input(GearBox), Assembly = find(GearBox, Input),
    erlmachine_transmission:transmit(Assembly, Motion).

-spec transmit(GearBox::assembly(), Motion::term(), TimeOut::integer()) ->
                      success(term()) | failure(term(), term()).
transmit(GearBox, Motion, TimeOut) ->
    Input = input(GearBox), Assembly = find(GearBox, Input),
    erlmachine_transmission:transmit(Assembly, Motion, TimeOut).

-spec attach(GearBox::assembly(), Part::assembly()) ->
                    success(term()) | failure(term(), term()).
attach(GearBox, Part) ->
    Output = output(GearBox), Assembly = find(GearBox, Output),
    erlmachine_transmission:attach(Assembly, Part).

-spec detach(GearBox::assembly(), ID::serial_no()) -> 
                    success(term()) | failure(term(), term()).
detach(GearBox, ID) ->
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

-spec input(GearBox::assembly(), Input::assembly()) -> Release::assembly().
input(GearBox, Input) ->
    Product = erlmachine_assembly:product(GearBox),
    SN = erlmachine_assembly:serial_no(Input),
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

-spec output(GearBox::assembly(), Output::assembly()) -> Release::assembly().
output(GearBox, Output) ->
    Product = erlmachine_assembly:product(GearBox),
    SN = erlmachine_assembly:serial_no(Output),
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

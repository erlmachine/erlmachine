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
-export([gearbox/1]).

-export([master/1]).

-export([rotate/2]).
-export([transmit/2]).
-export([connect/3, disconnect/2]).

%% erlmachine_assembly
-export([install/1, uninstall/2, attach/3, detach/2]).
-export([installed/2, uninstalled/3, attached/3, detached/3]).

%% erlmachine_factory
-export([accept/2]).
-export([accepted/3, rejected/3]).

%% erlmachine_system
-export([form/1, submit/2]).
-export([blocked/4, overloaded/3]).

-export([
         input/1, input/2,
         env/1, env/2,
         output/1, output/2
        ]).

-export([mounted/2]).
-export([parts/2, parts/3, parts/4]).

-export([find/2]).
-export([in/2, out/2]).

-export([components/1]).
-export([attachments/1]).

-export([record_name/0, attributes/0]).

-include("erlmachine_factory.hrl").
-include("erlmachine_system.hrl").

%% erlmachine_assembly callbacks;
%% Requests:
-callback install(Label::term(), Ids::list(term()), Schema::term(), Opt::term(), Env::list()) -> 
    success(term()) | failure(term(), term(), term()) | failure(term()).

-callback uninstall(Label::term(), Reason::term(), Schema::term()) -> 
    success() | failure(term(), term(), term()).

-callback attach(Label::term(), Reg::term(), Id::term(), Schema::term()) -> 
    success(term()) | failure(term(), term(), term()) | failure(term()).

-callback detach(Label::term(), Id::term(), Schema::term()) -> 
    success(term()) | failure(term(), term(), term()) | failure(term()).

%% Events:
-callback installed(GearBox::assembly(), Part::assembly()) -> 
    success().

-callback uninstalled(GearBox::assembly(), Part::assembly(), Reason::term()) ->
    success().

-callback attached(GearBox::assembly(), Part::assembly(), Ext::assembly()) ->
    success().

-callback detached(GearBox::assembly(), Part::assembly(), Ext::assembly()) -> 
    success().

%% erlmachine_factory callbacks;
%% Requests:
-callback accept(Label::term(), Criteria::criteria(), Schema::term()) -> 
    success(term(), term()) | failure(term(), term(), term()).

%% Events:
-callback accepted(GearBox::assembly(), Ext::assembly(), Criteria::criteria()) -> 
    success().

-callback rejected(GearBox::assembly(), Ext::assembly(), Criteria::criteria()) -> 
    success().

%% erlmachine_system callbacks;
%% Requests:
-callback form(Label::term(), Schema::term()) ->
    success(term(), term()) | failure(term(), term(), term()) | failure(term()).

-callback submit(Label::term(), Form::term(), Schema::term()) ->
    success(term()) | failure(term(), term(), term()) | failure(term()).

%% Events:
-callback blocked(GearBox::assembly(), Part::assembly(), E::term(), R::term()) ->
    success().

-callback overloaded(GearBox::assembly(), Part::assembly(), Load::term()) ->
    success().

-optional_callbacks([attach/4, detach/3, installed/2, uninstalled/3, attached/3, detached/3]).

-optional_callbacks([accepted/3, rejected/3]).

-optional_callbacks([form/2, submit/3, blocked/4, overloaded/3]).

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

-spec master(GearBox::assembly()) -> Release::assembly().
master(GearBox) ->
    Schema = erlmachine_assembly:schema(),
    erlmachine_assembly:schema(GearBox, Schema).

-spec installed(GearBox::assembly(), Part::assembly()) ->
                       success().
installed(GearBox, Part) ->
    ModelName = erlmachine_assembly:model_name(GearBox),

    Mod = ModelName, Fun = installed, Args = [GearBox, Part],
    erlmachine:optional_callback(Mod, Fun, Args, ok),
    ok.

-spec uninstalled(GearBox::assembly(), Part::assembly(), Reason::term()) ->
                         success().
uninstalled(GearBox, Part, Reason) ->
    ModelName = erlmachine_assembly:model_name(GearBox),

    Mod = ModelName, Fun = uninstalled, Args = [GearBox, Part, Reason],
    erlmachine:optional_callback(Mod, Fun, Args, ok),
    ok.

-spec overloaded(GearBox::assembly(), Part::assembly(), Load::term()) ->
                        success().
overloaded(GearBox, Part, Load) ->
    ModelName = erlmachine_assembly:model_name(GearBox),

    Mod = ModelName, Fun = overloaded, Args = [GearBox, Part, Load],
    erlmachine:optional_callback(Mod, Fun, Args, ok),
    ok.

-spec blocked(GearBox::assembly(), Part::assembly(), E::term(), R::term()) ->
                     success().
blocked(GearBox, Part, E, R) ->
    ModelName = erlmachine_assembly:model_name(GearBox),

    Mod = ModelName, Fun = blocked, Args = [GearBox, Part, E, R],
    erlmachine:optional_callback(Mod, Fun, Args, ok),
    ok.

-spec attached(GearBox::assembly(), Part::assembly(), Ext::assembly()) ->
                      success().
attached(GearBox, Part, Ext) ->
    ModelName = erlmachine_assembly:model_name(GearBox),

    Mod = ModelName, Fun = attached, Args = [GearBox, Part, Ext],
    erlmachine:optional_callback(Mod, Fun, Args, ok),
    ok.

-spec detached(GearBox::assembly(), Part::assembly(), Ext::assembly()) ->
                      success().
detached(GearBox, Part, Ext) ->
    ModelName = erlmachine_assembly:model_name(GearBox),

    Mod = ModelName, Fun = detached, Args = [GearBox, Part, Ext],
    erlmachine:optional_callback(Mod, Fun, Args, ok),
    ok.

-spec accepted(GearBox::assembly(), Ext::assembly(), Criteria::criteria()) -> 
                      success().
accepted(GearBox, Ext, Criteria) ->
    ModelName = erlmachine_assembly:model_name(GearBox),

    Mod = ModelName, Fun = accepted, Args = [GearBox, Ext, Criteria],
    erlmachine:optional_callback(Mod, Fun, Args, ok),
    ok.

-spec rejected(GearBox::assembly(), Ext::assembly(), Criteria::criteria()) -> 
                      success().
rejected(GearBox, Ext, Criteria) ->
    ModelName = erlmachine_assembly:model_name(GearBox),

    Mod = ModelName, Fun = rejected, Args = [GearBox, Ext, Criteria],
    erlmachine:optional_callback(Mod, Fun, Args, ok),
    ok.

-spec install(GearBox::assembly()) -> 
                     success(assembly()) | failure(term(), term(), assembly()).
install(GearBox) ->
    Label = erlmachine_assembly:label(GearBox),

    Env = erlmachine_gearbox:env(GearBox), 
    Opt = erlmachine_assembly:model_options(GearBox),

    Parts = erlmachine_assembly:parts(GearBox),
    Ids = [erlmachine_assembly:label(Part)|| Part <- Parts],
    ModelName = erlmachine_assembly:model_name(GearBox),

    case ModelName:install(Label, Ids, state(GearBox), Opt, Env) of
        {ok, State} ->
            %% We are going to add error handling later;
            Rel = state(GearBox, State),
            {ok, Rel};
        {error, E, R, State} ->
            {error, E, R, state(GearBox, State)}
    end.

-spec attach(GearBox::assembly(), Reg::term(), Ext::assembly()) ->
                    success(assembly()) | failure(term(), term(), assembly()).
attach(GearBox, Reg, Ext) ->
    ModelName = erlmachine_assembly:model_name(GearBox),
    Label = erlmachine_assembly:label(GearBox), Id = erlmachine_assembly:label(Ext),
 
    case ModelName:attach(Label, Reg, Id, state(GearBox)) of
        {ok, State} ->
            Part = erlmachine_assembly:mounted(Ext, GearBox),
            Rel = erlmachine_assembly:add(state(GearBox, State), Part),
            %% At that place we don't emit any events (cause is gearbox issue level);
            {ok, Part, Rel};
        {error, E, R, State} ->
            {error, E, R, state(GearBox, State)}
    end.

-spec detach(GearBox::assembly(), Id::term()) ->
                    success(assembly()) | failure(term(), term(), assembly()).
detach(GearBox, Id) ->
    Label = erlmachine_assembly:label(GearBox),
    ModelName = erlmachine_assembly:model_name(GearBox),

    case ModelName:detach(Label, Id, state(GearBox)) of
        {ok, State} ->
            Rel = erlmachine_assembly:remove(state(GearBox, State), Id),
            {ok, Rel};
        {error, E, R, State} ->
            {error, E, R, state(GearBox, State)}
    end.

-spec accept(GearBox::assembly(), Criteria::term()) ->
                    success()| failure(term(), term(), term()).
accept(GearBox, Criteria) ->
    Label = erlmachine_assembly:label(GearBox),
    ModelName = erlmachine_assembly:model_name(GearBox),

    case ModelName:accept(Label, Criteria, state(GearBox)) of
        {ok, Res, State} ->
            {ok, Res, state(GearBox, State)};
        {error, E, R, State} ->
            {error, E, R, state(GearBox, State)}
    end.

-spec uninstall(GearBox::assembly(), Reason::term()) -> 
                       ok.
uninstall(GearBox, Reason) ->
    Label = erlmachine_assembly:label(GearBox),
    ModelName = erlmachine_assembly:model_name(GearBox),

    {ok, State} = ModelName:uninstall(Label, Reason, state(GearBox)),
    {ok, state(GearBox, State)}.

-spec form(GearBox::assembly()) ->
                  success(term(), assembly()) | failure(term(), term(), term()).
form(GearBox) ->
    Label = erlmachine_assembly:label(GearBox),
    ModelName = erlmachine_assembly:model_name(GearBox),

    Mod = ModelName, Fun = form, Args = [Label, state(GearBox)],
    Def = erlmachine:success([], state(GearBox)),

    case erlmachine:optional_callback(Mod, Fun, Args, Def) of
        {ok, Form, State} ->
            {ok, Form, state(GearBox, State)};
        {error, E, R, State} ->
            {error, E, R, state(GearBox, State)} 
    end.

-spec submit(GearBox::assembly(), Form::term()) ->
                    success(term(), assembly()) | failure(term(), term(), term()).
submit(GearBox, Form) ->
    Label = erlmachine_assembly:label(GearBox),
    ModelName = erlmachine_assembly:model_name(GearBox),

    case ModelName:submit(Label, Form, state(GearBox)) of
        {ok, Res, State} ->
            {ok, Res, state(GearBox, State)};
        {error, E, R, State} ->
            {error, E, R, state(GearBox, State)} 
    end.

-spec rotate(GearBox::assembly(), Motion::term()) ->
                    Motion::term().
rotate(GearBox, Motion) ->
    erlmachine_transmission:rotate(GearBox, input(GearBox), Motion).

-spec transmit(GearBox::assembly(), Motion::term()) ->
                      success(term()) | failure(term(), term()).
transmit(GearBox, Motion) ->
    erlmachine_transmission:transmit(GearBox, input(GearBox), Motion).

-spec connect(GearBox::assembly(), Reg::term(), Part::assembly()) ->
                    success(term(), term()) | failure(term(), term()).
connect(GearBox, Reg, Part) ->
    Output = output(GearBox), 

    erlmachine_assembly:attach(GearBox, Output, Reg, Part).

-spec disconnect(GearBox::assembly(), Id::term()) -> 
                    success(term()) | failure(term(), term()).
disconnect(GearBox, Id) ->
    Output = output(GearBox),
    erlmachine_assembly:detach(GearBox, Output, Id).

%% We are going to provide access by path gearbox.shaft.# (like rabbitmq notation) too;

-spec find(GearBox::assembly(), Label::term()) -> 
                  assembly() | false.
find(GearBox, Label) ->
    Schema = erlmachine_assembly:schema(GearBox),
    erlmachine_schema:vertex(Schema, Label).

-spec in(GearBox::assembly(), Label::term()) -> list().
in(GearBox, Label) ->
    Schema = erlmachine_assembly:schema(GearBox),
    erlmachine_schema:in_edges(Schema, Label).

-spec out(GearBox::assembly(), Label::term()) -> list().
out(GearBox, Label) ->
    Schema = erlmachine_assembly:schema(GearBox),
    erlmachine_schema:out_edges(Schema, Label).

-spec components(GearBox::assembly()) -> list().
components(GearBox) ->
    Schema = erlmachine_assembly:schema(GearBox),
    erlmachine_schema:vertices(Schema).

-spec attachments(GearBox::assembly()) -> list().
attachments(GearBox) ->
    Schema = erlmachine_assembly:schema(GearBox),
    erlmachine_schema:edges(Schema).

-spec state(Axle::assembly()) -> term().
state(Axle) ->
    erlmachine_assembly:schema(Axle).

-spec state(Axle::assembly(), Schema::term()) -> 
                   assembly().
state(Axle, Schema) ->
    erlmachine_assembly:schema(Axle, Schema).

-spec input(GearBox::assembly()) -> 
                   term().
input(GearBox) ->
    Product = erlmachine_assembly:product(GearBox),
    Product#gearbox.input.

-spec input(GearBox::assembly(), Id::term()) ->
                   assembly().
input(GearBox, Id) ->
    Product = erlmachine_assembly:product(GearBox),
    erlmachine_assembly:product(GearBox, Product#gearbox{ input=Id }).

-spec output(GearBox::assembly()) -> 
                    term().
output(GearBox) ->
    Product = erlmachine_assembly:product(GearBox),
    Product#gearbox.output.

-spec output(GearBox::assembly(), Id::term()) -> 
                    assembly().
output(GearBox, Id) ->
    Product = erlmachine_assembly:product(GearBox),
    erlmachine_assembly:product(GearBox, Product#gearbox{ output=Id }).

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
    Rel = parts(GearBox, Parts),
    input(Rel, erlmachine:label(Input)).

-spec parts(GearBox::assembly(), Input::assembly(), Parts::list(assembly()), Output::assembly()) ->
                   assembly().
parts(GearBox, Input, Parts, Output) ->
    Rel = parts(GearBox, Parts),
    output(input(Rel, erlmachine:label(Input)), erlmachine:label(Output)).

-spec gearbox(Env::term()) -> gearbox().
gearbox(Env) ->
    #gearbox{ env=Env }.

-spec mounted(GearBox::assembly(), Parts::list(assembly())) -> Release::assembly().
mounted(GearBox, Parts) ->
    Mounted = [erlmachine_assembly:mounted(Part, GearBox)|| Part <- Parts],
    erlmachine_assembly:parts(GearBox, Mounted).

%% processes need to be instantiated by builder before;

%% detached;
%% (Reason == normal) orelse erlmachine_system:crash(Assembly, Reason),

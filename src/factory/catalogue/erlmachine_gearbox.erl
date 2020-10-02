-module(erlmachine_gearbox).
%% This is a decoration module which can supply addtional data about gearbox assembly;
%% Gearbox is a component which is responsible for reliable spatial placement for the all processes (extensions);
%% There is the place where shafts, gears and axles are located. (The main container around the transmission);
%% The gearbox is divided on so called stages (stage is a torgue between two adjacent workers);
%% NOTE Only process (driver) that created gearbox is allowed to manage internal schema;

%% There are two kinds of behaviours, erlmachine_supervisor and erlmachine_worker;
%% Supervisors are ususally arranged by layered structure;
%% Each container (axle or gearbox) can be notified about its own containerized parts;
%% That's all very similar to the modern orchestration approach like k8s, swarm, etc..

%% NOTE Not only event callbacks available for supervisor model (I guess the main methods too). The purpouse is to manage attachment policies, etc.;
-export([gearbox/3]).

-export([install/1, install/2, uninstall/1, uninstall/2]).

-export([type/0]).

-include("erlmachine_factory.hrl").
-include("erlmachine_system.hrl").

%% We can supply different protocols via decorator module;
-spec gearbox(Schema::term(), Body::term(), Model::model(), Env::term()) -> assembly().
gearbox(Schema, Body, Model, Env) ->
    Assembly = erlmachine_assembly:assembly(?MODULE, Schema, Body, Model, Env),
    erlmachine_assembly:tag(Assembly, type()).

-spec install(GearBox::assembly()) ->
                     success(pid()) | failure(term(), term()).
install(GearBox) ->
    erlmachine_supervisor_prototype:init(GearBox).

-spec install(GearBox::assembly(), Ext::assembly()) ->
                     success(pid()) | failure(term(), term()).
install(GearBox, Ext) ->
    erlmachine_supervisor_prototype:start_child(GearBox, Ext).

-spec uninstall(Assembly::assembly(), Id::term()) -> 
                       success() | failure(term(), term()).
uninstall(Assembly, Id) ->
    erlmachine_supervisor_prototype:terminate_child(Assembly, Id).

-spec uninstall(GearBox::assembly()) -> 
                       success().
uninstall(GearBox) ->
    erlmachine_supervisor_prototype:terminate(GearBox).

-spec attach(GearBox::assembly(), Reg::term(), Ext::assembly()) ->
                    success(assembly()) | failure(term(), term()).
attach(GearBox, Reg, Ext) ->
    ModelName = erlmachine_assembly:model_name(GearBox),
    Label = erlmachine_assembly:label(GearBox), Id = erlmachine_assembly:label(Ext),
 
    case ModelName:attach(Label, Reg, Id, state(GearBox)) of
        {ok, State} ->
            Part = erlmachine_assembly:mounted(Ext, GearBox),
            Rel = erlmachine_assembly:add(state(GearBox, State), Part),
            %% At that place we don't emit any events (cause is gearbox issue level);
            erlmachine:success(Part, Rel);
        {error, E, R, State} ->
            erlmachine:failure(E, R, state(GearBox, State))
    end.

-spec detach(GearBox::assembly(), Id::term()) ->
                    success(term()) | failure(term(), term(), term()).
detach(GearBox, Id) ->
    Label = erlmachine_assembly:label(GearBox),
    ModelName = erlmachine_assembly:model_name(GearBox),

    case ModelName:detach(Label, Id, state(GearBox)) of
        {ok, State} ->
            Rel = erlmachine_assembly:remove(state(GearBox, State), Id),
            erlmachine:success(Rel);
        {error, E, R, State} ->
            erlmachine:failure(E, R, state(GearBox, State))
    end.

-spec type() -> atom().
type() ->
    'supervisor'.

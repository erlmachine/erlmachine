-module(erlmachine_gearbox).
%% NOTE: This is a decoration module which enriches assembly by gearbox related data;
%% Gearbox is a component which is responsible for reliable spatial placement of the whole transmission;
%% There is the place where shafts, gears and axles have to be located. 
%% This module is the main container around the whole transmission;
%% The gearbox is divided on so called stages;
%% NOTE Only process (driver) that created gearbox is allowed to manage internal schema;

%% That's all very similar to the modern orchestration approach like k8s, swarm, etc..
%% Where each contained assembly represensts microservice and gearbox acts as orchestration env;

-export([gearbox/3]).

-export([install/1, install/2, uninstall/1, uninstall/2]).

-export([type/0]).

-include("erlmachine_factory.hrl").
-include("erlmachine_system.hrl").

-spec gearbox(Schema::term(), Model::model(), Env::term()) -> assembly().
gearbox(Schema, Model, Env) ->
    %% TODO: To support Body by additional metadata;
    Body = [],
    Assembly = erlmachine_assembly:assembly(?MODULE, Schema, Body, Model, Env),
    erlmachine_assembly:tag(Assembly, type()).

-spec install(GearBox::assembly()) ->
                     success(pid()) | failure(term(), term()).
install(GearBox) ->
    erlmachine_supervisor_prototype:install(GearBox).

-spec install(GearBox::assembly(), Ext::assembly()) ->
                     success(pid()) | failure(term(), term()).
install(GearBox, Ext) ->
    erlmachine_supervisor_prototype:install(GearBox, Ext).

-spec uninstall(Assembly::assembly(), Id::term()) -> 
                       success().
uninstall(Assembly, Id) ->
    erlmachine_supervisor_prototype:uninstall(Assembly, Id).

-spec uninstall(GearBox::assembly()) -> 
                       success().
uninstall(GearBox) ->
    erlmachine_supervisor_prototype:uninstall(GearBox).

-spec type() -> atom().
type() ->
    'supervisor'.

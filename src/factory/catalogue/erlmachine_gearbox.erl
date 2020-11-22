-module(erlmachine_gearbox).
%% NOTE: This is a decoration module which enriches assembly by gearbox related data;
%% Gearbox is a component which is responsible for reliable spatial placement of the whole transmission;
%% There is the place where shafts, gears and axles have to be located. 
%% This module is the main container around the whole transmission;
%% The gearbox is divided on so called stages;
%% NOTE Only process (driver) that created gearbox is allowed to manage internal schema;
%% NOTE: GearBox is labeled by 'root' by default;

%% That's all very similar to the modern orchestration approach like k8s, swarm, etc..
%% Where each contained assembly represensts microservice and gearbox acts as orchestration env;

-export([gearbox/2]).

-export([start/1]).
-export([install/2, uninstall/2]).
-export([stop/1]).

-export([type/0]).
-export([prefix/0]).

-type model() :: erlmachine_model:model().

-include("erlmachine_assembly.hrl").
-include("erlmachine_system.hrl").

-spec gearbox(Model::model(), Env::term()) -> assembly().
gearbox(Model, Env) ->
    %% TODO: To support Body by additional metadata;
    Body = [],
    Assembly = erlmachine_assembly:assembly(?MODULE, Body, Model, Env),
    erlmachine:label(erlmachine:tag(Assembly, type()), 'root').

-spec start(GearBox::assembly()) ->
                     success(pid()) | failure(term(), term()).
start(GearBox) ->
    erlmachine_supervisor_prototype:start(GearBox).

-spec install(GearBox::assembly(), Ext::assembly()) ->
                     success(pid()) | failure(term(), term()).
install(GearBox, Ext) ->
    erlmachine_supervisor_prototype:install(GearBox, Ext).

-spec uninstall(Assembly::assembly(), Id::term()) -> 
                       success().
uninstall(Assembly, Id) ->
    erlmachine_supervisor_prototype:uninstall(Assembly, Id).

-spec stop(GearBox::assembly()) -> 
                       success().
stop(GearBox) ->
    erlmachine_supervisor_prototype:stop(GearBox).

-spec type() -> atom().
type() ->
    'supervisor'.

-spec prefix() -> binary().
prefix() ->
    <<"GX-">>.

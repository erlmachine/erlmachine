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

-export([new/0]).

-export([type/0]).
-export([prefix/0]).

-include("erlmachine_assembly.hrl").
-include("erlmachine_system.hrl").

-spec new() -> assembly().
new() ->
    %% TODO: To support Body by additional metadata;
    Socket = <<"*">>,
    Body = #{},
    Tags = [type()], Desc = <<"">>,
    erlmachine_assembly:assembly(?MODULE, Socket, Body, Tags, Desc).

%% NOTE: We should check this call when schema is requested;
-spec type() -> atom().
type() ->
    'supervisor'.

-spec prefix() -> binary().
prefix() ->
    <<"GX">>.

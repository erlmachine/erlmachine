-module(erlmachine_gearbox).
%% NOTE: This is a decoration module which enriches assembly by gearbox related data;
%% Gearbox is a container which stores the all extensions set and represents a root supervisor of a cluster.

%% That's all very similar to the modern orchestration approaches like k8s, swarm, etc..
%% Whereas each contained assembly represensts microservice and gearbox keeps cluster environment inside;

-export([new/1]).
-export([type/0]).

-include_lib("erlbox/include/erlbox.hrl").

-include("erlmachine_assembly.hrl").

-spec new(assembly()) -> assembly(). %% Default representation;
new(Assembly) ->
    %% TODO: Additional decoration inside body;
    Body = #{}, Port = <<"*">>,
    Rel = erlmachine_assembly:body(erlmachine_assembly:type(Assembly, _Type = type()), Body),
    erlmachine_assembly:port(Rel, Port).

%% NOTE: We should check this call when schema is requested;
-spec type() -> atom().
type() ->
    'supervisor'.

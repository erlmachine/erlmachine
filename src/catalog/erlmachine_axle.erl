-module(erlmachine_axle).
%% NOTE: This is a decoration module which enriches assembly by axle related data;
%% Axle is a component which is responsible for monitoring of it's own extensions;
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

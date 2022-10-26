-module(erlmachine_shaft).
%% NOTE: This is a decoration module which enriches assembly by shaft related data;
%% Shaft represents a worker which routes a load through a transmission;
%% Shaft is called multiple times per message (extensions count);

-export([new/1]).
-export([type/0]).

-include_lib("erlbox/include/erlbox.hrl").

-include("erlmachine_assembly.hrl").

-spec new(assembly()) -> assembly(). %% Default representation;
new(Assembly) ->
    %% TODO: Additional decoration inside body;
    Body = #{}, Port = <<"#">>,
    Rel = erlmachine_assembly:body(erlmachine_assembly:type(Assembly, _Type = type()), Body),
    erlmachine_assembly:port(Rel, Port).

-spec type() -> atom().
type() ->
    'worker'.

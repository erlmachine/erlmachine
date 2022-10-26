-module(erlmachine_gear).
%% NOTE: This is a decoration module which enriches assembly by gear related data;
%% Gear represents a worker which supplies a load through a transmission;
%% Gear is called one time per message;

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

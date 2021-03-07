-module(erlmachine_gear).
%% NOTE: This is a decoration module which enriches assembly by gear related data;
%% Gear represents a worker which supplies a load through a transmission;
%% Gear is called one time per message;

%% TODO: Decorated module should provide meta data about extension appearence on a canvas
-export([new/0]).

-export([type/0]).
-export([prefix/0]).

-include("erlmachine_assembly.hrl").
-include("erlmachine_system.hrl").

-spec new() -> assembly(). %% Default representation;
new() ->
    %% TODO: Additional decoration within body;
    Socket = <<"#">>,
    Body = #{},
    Tags = [type()], Desc = <<"">>,
    erlmachine_assembly:assembly(?MODULE, Socket, Body, Tags, Desc).

-spec type() -> atom().
type() ->
    'worker'.

-spec prefix() -> binary().
prefix() ->
    <<"GR">>.

-module(erlmachine_axle).
%% NOTE: This is a decoration module which enriches assembly by axle related data;
%% Axle is a component which is responsible for monitoring of the placed transmission parts;
-export([new/0]).

-export([type/0]).
-export([prefix/0]).

-include("erlmachine_assembly.hrl").
-include("erlmachine_system.hrl").

-spec new() -> assembly().
new() ->
    %% TODO; To decorate Body by additional data;
    Socket = <<"*">>,
    Body = [],
    Tags = [type()], Desc = <<"">>,
    erlmachine_assembly:assembly(?MODULE, Socket, Body, Tags, Desc).

-spec type() -> atom().
type() ->
    'supervisor'.

-spec prefix() -> binary().
prefix() ->
    <<"AE">>.

-module(erlmachine_shaft).
%% NOTE: This is a decoration module which enriches assembly by shaft related data;
%% Shaft represents a worker which routes a load through a transmission;
%% Shaft is called multiple times per message (extensions count);

-export([new/0]).

-export([type/0]).
-export([prefix/0]).

-include("erlmachine_assembly.hrl").
-include("erlmachine_system.hrl").

-spec new() -> assembly().
new() ->
    %% TODO: To decorate Body by additional metadata;
    Socket = <<"#">>,
    Body = [],
    Tags = [type()], Desc = <<"">>,
    erlmachine_assembly:assembly(?MODULE, Socket, Body, Tags, Desc).

-spec type() -> atom().
type() ->
    'worker'.

-spec prefix() -> binary().
prefix() ->
    <<"ST">>.

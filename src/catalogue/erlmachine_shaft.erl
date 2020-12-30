-module(erlmachine_shaft).
%% NOTE: This is a decoration module which enriches assembly by shaft related data;
%% Shaft represents a worker which routes a load through a transmission;
%% Shaft is called multiple times per message (extensions count);

-export([shaft/0]).

-export([boot/1]).
-export([process/2]).
-export([execute/2]).
-export([shutdown/3]).

-export([type/0]).
-export([prefix/0]).

-include("erlmachine_assembly.hrl").
-include("erlmachine_system.hrl").

-spec shaft() -> assembly().
shaft() ->
    %% TODO: To decorate Body by additional metadata;
    Socket = <<"#">>,
    Body = [],
    Tags = [type()], Desc = <<"">>,
    erlmachine_assembly:assembly(?MODULE, Socket, Body, Tags, Desc).

-spec boot(Shaft::assembly()) ->
                     success(term()) | failure(term(), term()).
boot(Shaft) ->
    erlmachine_worker_prototype:boot(Shaft).

-spec process(Shaft::assembly(), Motion::term()) ->
                    success().
process(Shaft, Motion) ->
    erlmachine_worker_prototype:process(Shaft, Motion).

-spec execute(Shaft::assembly(), Motion::term()) ->
                      success(term(), assembly()) | failure(term(), term(), term()).
execute(Shaft, Motion) ->
    erlmachine_worker_prototype:execute(Shaft, Motion).

-spec shutdown(Shaft::assembly(), Reason::term(), Timeout::term()) ->
                       success().
shutdown(Shaft, Reason, Timeout) ->
    erlmachine_worker_prototype:shutdown(Shaft, Reason, Timeout).

-spec type() -> atom().
type() ->
    'worker'.

-spec prefix() -> binary().
prefix() ->
    <<"ST">>.

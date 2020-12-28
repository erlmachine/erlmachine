-module(erlmachine_gear).
%% NOTE: This is a decoration module which enriches assembly by gear related data;
%% Gear represents a worker which supplies a load through a transmission;
%% Gear is called one time per message;

-export([gear/0]).

-export([boot/1]).
-export([process/2]).
-export([execute/2]).
-export([shutdown/3]).

-export([type/0]).
-export([prefix/0]).

-include("erlmachine_assembly.hrl").
-include("erlmachine_system.hrl").

-spec gear() -> assembly(). %% Default representation;
gear() ->
    %% TODO: Additional decoration within body;
    Body = #{},
    Tags = [type()], Desc = <<"">>,
    erlmachine_assembly:assembly(?MODULE, Body, Tags, Desc).

-spec boot(Gear::assembly()) ->
                     success(pid()) | failure(term(), term()).
boot(Gear) ->
    erlmachine_worker_prototype:boot(Gear).

-spec process(Gear::assembly(), Motion::term()) ->
                    success().
process(Gear, Motion) ->
    erlmachine_worker_prototype:process(Gear, Motion).

-spec execute(Gear::assembly(), Motion::term()) ->
                      term() | failure(term(), term()).
execute(Gear, Motion) ->
    erlmachine_worker_prototype:execute(Gear, Motion).

-spec shutdown(Gear::assembly(), Reason::term(), Timeout::term()) -> 
                       success().
shutdown(Gear, Reason, Timeout) ->
    erlmachine_worker_prototype:shutdown(Gear, Reason, Timeout).

-spec type() -> atom().
type() ->
    'worker'.

-spec prefix() -> binary().
prefix() ->
    <<"GR-">>. 

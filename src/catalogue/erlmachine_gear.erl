-module(erlmachine_gear).
%% NOTE: This is a decoration module which enriches assembly by gear related data;
%% Gear represents a worker which supplies a load through a transmission;
%% Gear is called one time per message;

-export([gear/0]).

-export([start/1]).
-export([rotate/2, transmit/2]).
-export([stop/1]).

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

-spec start(Gear::assembly()) ->
                     success(pid()) | failure(term(), term()).
start(Gear) ->
    erlmachine_worker_prototype:start(Gear).

-spec rotate(Gear::assembly(), Motion::term()) ->
                    success().
rotate(Gear, Motion) ->
    erlmachine_worker_prototype:rotate(Gear, Motion).

-spec transmit(Gear::assembly(), Motion::term()) ->
                      term() | failure(term(), term()).
transmit(Gear, Motion) ->
    erlmachine_worker_prototype:transmit(Gear, Motion).

-spec stop(Gear::assembly()) -> 
                       success().
stop(Gear) ->
    erlmachine_worker_prototype:stop(Gear).

-spec type() -> atom().
type() ->
    'worker'.

-spec prefix() -> binary().
prefix() ->
    <<"GR-">>. 

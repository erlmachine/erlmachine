-module(erlmachine_gear).
%% NOTE: This is a decoration module which enriches assembly by gear related data;
%% Gear represents a worker which supplies a load through a transmission;
%% Gear is called one time per message;

-export([gear/1]).

-export([install/1, rotate/1, transmit/1,  uninstall/2]).

-export([type/0]).

-include("erlmachine_factory.hrl").
-include("erlmachine_system.hrl").

-spec gear(Model::model()) -> assembly(). %% Default representation;
gear(Model) ->
    %% TODO: Additional decoration within body;
    Body = #{},
    Assembly = erlmachine_assembly:assembly(?MODULE, Body, Model),
    erlmachine_assembly:tag(Assembly, type()).

-spec install(Gear::assembly()) -> 
                     success(pid()) | failure(term(), term()).
install(Gear) ->
    erlmachine_worker_prototype:install(Gear).

-spec rotate(Gear::assembly(), Motion::term(), Ext::assembly()) ->
                    success().
rotate(Gear, Motion, Ext) ->
    erlmachine_worker_prototype:rotate(Gear, Motion, Ext).

-spec transmit(Gear::assembly(), Motion::term()) ->
                      term() | failure(term(), term()).
transmit(Gear, Motion) ->
    erlmachine_worker_prototype:transmit(Gear, Motion).

-spec uninstall(Gear::assembly()) -> 
                       success().
uninstall(Gear) ->
    erlmachine_worker_prototype:uninstall(Gear).

-spec type() -> atom().
type() ->
    'worker'.

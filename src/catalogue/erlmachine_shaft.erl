-module(erlmachine_shaft).
%% NOTE: This is a decoration module which enriches assembly by shaft related data;
%% Shaft represents a worker which routes a load through a transmission;
%% Shaft is called multiple times per message (extensions count);

-export([shaft/0]).

-export([install/1, rotate/2, transmit/2,  uninstall/1]).

-export([type/0]).
-export([prefix/0]).

-include("erlmachine_assembly.hrl").
-include("erlmachine_system.hrl").

-spec shaft() -> assembly().
shaft() ->
    %% TODO: To decorate Body by additional metadata;
    Body = [],
    Tags = [type()], Desc = <<"">>,
    erlmachine_assembly:assembly(?MODULE, Body, Tags, Desc).

-spec install(Shaft::assembly()) ->
                     success(term()) | failure(term(), term()).
install(Shaft) ->
    erlmachine_worker_prototype:install(Shaft).

-spec rotate(Shaft::assembly(), Motion::term()) ->
                    success().
rotate(Shaft, Motion) ->
    erlmachine_worker_prototype:rotate(Shaft, Motion).

-spec transmit(Shaft::assembly(), Motion::term()) ->
                      success(term(), assembly()) | failure(term(), term(), term()).
transmit(Shaft, Motion) ->
    erlmachine_worker_prototype:transmit(Shaft, Motion).

-spec uninstall(Shaft::assembly()) ->
                       success().
uninstall(Shaft) ->
    erlmachine_worker_prototype:uninstall(Shaft).

-spec type() -> atom().
type() ->
    'worker'.

-spec prefix() -> binary().
prefix() ->
    <<"ST-">>.

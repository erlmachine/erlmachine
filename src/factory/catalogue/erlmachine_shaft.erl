-module(erlmachine_shaft).
%% NOTE: This is a decoration module which enriches assembly by shaft related data;
%% Shaft represents a worker which routes a load through a transmission;
%% Shaft is called multiple times per message (extensions count);

-export([shaft/1]).

-export([install/1, rotate/3, transmit/2,  uninstall/1]).

-export([type/0]).
-export([prefix/0]).

-type model() :: erlmachine_model:model().

-include("erlmachine_assembly.hrl").
-include("erlmachine_system.hrl").

-spec shaft(Model::model()) -> assembly().
shaft(Model) ->
    %% TODO: To decorate Body by additional metadata;
    Body = [],
    Assembly = erlmachine_assembly:assembly(?MODULE, Body, Model),
    erlmachine:tag(Assembly, type()).

-spec install(Shaft::assembly()) ->
                     success(term()) | failure(term(), term()).
install(Shaft) ->
    erlmachine_worker_prototype:install(Shaft).

-spec rotate(Shaft::assembly(), Motion::term(), Ext::assembly()) ->
                    success().
rotate(Shaft, Motion, Ext) ->
    erlmachine_worker_prototype:rotate(Shaft, Motion, Ext).

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

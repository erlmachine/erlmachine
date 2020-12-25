-module(erlmachine_axle).
%% NOTE: This is a decoration module which enriches assembly by axle related data;
%% Axle is a component which is responsible for monitoring of the placed transmission parts;
-export([axle/0]).

-export([start/1]).
-export([install/2, uninstall/2]).
-export([stop/1]).

-export([type/0]).
-export([prefix/0]).

-include("erlmachine_assembly.hrl").
-include("erlmachine_system.hrl").

-spec axle() -> assembly().
axle() ->
    %% TODO; To decorate Body by additional data;
    Body = [],
    Tags = [type()], Desc = <<"">>,
    erlmachine_assembly:assembly(?MODULE, Body, Tags, Desc).

-spec start(Axle::assembly()) -> 
                     success(pid()) | failure(term(), term()).
start(Axle) ->
    erlmachine_supervisor_prototype:start(Axle).

-spec install(Axle::assembly(), Ext::assembly()) ->
                    success(pid()) | failure(term(), term()).
install(Axle, Ext) ->
    erlmachine_supervisor_prototype:install(Axle, Ext).

-spec uninstall(Axle::assembly(), Id::term()) -> 
                       success().
uninstall(Axle, Id) ->
    erlmachine_supervisor_prototype:uninstall(Axle, Id).

-spec stop(Axle::assembly()) -> 
                       success().
stop(Axle) ->
    erlmachine_supervisor_prototype:stop(Axle).

-spec type() -> atom().
type() ->
    'supervisor'.

-spec prefix() -> binary().
prefix() ->
    <<"AE-">>.

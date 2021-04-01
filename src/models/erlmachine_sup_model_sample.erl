-module(erlmachine_sup_model_sample).

-behaviour(erlmachine_supervisor_model).
%% Can be widely used as service versioning;
-vsn(['sample']).

-export([boot/4]).
-export([install/2, uninstall/2]).

-include("erlmachine_user.hrl").
-include("erlmachine_factory.hrl").
-include("erlmachine_assembly.hrl").
-include("erlmachine_system.hrl").
-include("erlmachine_transmission.hrl").

-spec startup(UID::uid(), Vs::[vertex()], Opt::[term()], Env::map()) ->
    success() | failure(term(), term()).
startup(UID, Vs, Opt, Env) ->
%% TODO: To provide test cases parametrization through Env;
    io:format("~n~p:startup(~p, ~p, ~p, ~p)~n", [?MODULE, UID, Vs, Opt, Env]),
    erlmachine:success().

-spec install(UID::uid(), Spec::spec()) ->
    success() | failure(term(), term()).
install(UID, Spec) ->
    io:format("~n~p:install(~p, ~p)~n", [?MODULE, UID, Spec]),
    erlmachine:success().

-spec uninstall(UID::uid(), ID::term()) ->
    success() | failure(term(), term()).
uninstall(UID, ID) ->
    io:format("~n~p:uninstall(~p, ~p)~n", [?MODULE, UID, ID]),
    erlmachine:success().


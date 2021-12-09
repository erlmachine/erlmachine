-module(erlmachine_sup_model_sample).

-behaviour(erlmachine_supervisor_model).
%% Can be widely used as service versioning;
-vsn(['sample']).

-export([startup/4]).
-export([install/2, uninstall/2]).

-include("erlmachine_user.hrl").
-include("erlmachine_graph.hrl").
-include("erlmachine_system.hrl").

-spec startup(UID::uid(), Vs::[vertex()], Opt::map(), Env::map()) ->
    success() | failure(term(), term()).
startup(_UID, _Vs, _Opt, _Env) ->
%% TODO: To provide test cases parametrization through Env;
    erlmachine:success().

-spec install(UID::uid(), V::vertex()) ->
    success() | failure(term(), term()).
install(_UID, _V) ->
    erlmachine:success().

-spec uninstall(UID::uid(), V::vertex()) ->
    success() | failure(term(), term()).
uninstall(_UID, _V) ->
    erlmachine:success().


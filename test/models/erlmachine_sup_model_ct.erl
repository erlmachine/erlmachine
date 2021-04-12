-module(erlmachine_sup_model_ct).

-behaviour(erlmachine_supervisor_model).
%% Can be widely used as service versioning;
-vsn(['test']).

-export([startup/4]).

-export([install/2, uninstall/2]).

-export([shutdown/2]).

-include("erlmachine_user.hrl").
-include("erlmachine_graph.hrl").
-include("erlmachine_system.hrl").

-spec startup(UID::uid(), Vs::[vertex()], Opt::[term()], Env::map()) -> 
    success() | failure(term(), term()).
startup(UID, Vs, Opt, Env) ->
%% TODO: To provide test cases parametrization through Env;
    io:format(user, "~n~p:startup(~p, ~p, ~p, ~p)~n", [?MODULE, UID, Vs, Opt, Env]),
    erlmachine:success().

-spec install(UID::uid(), V::vertex()) -> 
    success() | failure(term(), term()).
install(UID, V) ->
    io:format(user, "~n~p:install(~p, ~p)~n", [?MODULE, UID, V]),
    erlmachine:success().

-spec uninstall(UID::uid(), V::vertex()) ->
    success() | failure(term(), term()).
uninstall(UID, V) ->
    io:format(user, "~n~p:uninstall(~p, ~p)~n", [?MODULE, UID, V]),
    erlmachine:success().

-spec shutdown(UID::uid(), Reason::term()) ->
    success() | failure(term(), term()).
shutdown(UID, Reason) ->
    io:format(user, "~n~p:shutdown(~p, ~p)~n", [?MODULE, UID, Reason]),
    erlmachine:success().


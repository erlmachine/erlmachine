-module(erlmachine_sup_model_ct).
%% Can be widely used as service versioning;
-vsn(['test']).

-export([boot/4]).
-export([install/2]).
-export([uninstall/2]).
-export([shutdown/2]).

-include("erlmachine_user.hrl").
-include("erlmachine_factory.hrl").
-include("erlmachine_assembly.hrl").
-include("erlmachine_system.hrl").
-include("erlmachine_transmission.hrl").

-spec boot(UID::uid(), Specs::[spec()], Opt::list(), Env::list()) -> 
    success() | failure(term(), term()).
boot(UID, Specs, Opt, Env) ->
%% TODO: To provide test cases parametrization through Env;
    ct:log("~n~p:boot(~p, ~p, ~p, ~p)~n", [?MODULE, UID, Specs, Opt, Env]),
    erlmachine:success().

-spec install(UID::uid(), Spec::spec()) -> 
    success() | failure(term(), term()).
install(UID, Spec) ->
    ct:log("~n~p:install(~p, ~p)~n", [?MODULE, UID, Spec]),
    erlmachine:success().

-spec uninstall(UID::uid(), ID::term()) ->
    success() | failure(term(), term()).
uninstall(UID, ID) ->
    ct:log("~n~p:uninstall(~p, ~p)~n", [?MODULE, UID, ID]),
    erlmachine:success().

-spec shutdown(UID::uid(), Reason::term()) ->
    success() | failure(term(), term()).
shutdown(UID, Reason) ->
    ct:log("~n~p:shutdown(~p, ~p)~n", [?MODULE, UID, Reason]),
    erlmachine:success().


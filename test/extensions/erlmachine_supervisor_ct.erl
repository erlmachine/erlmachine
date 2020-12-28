-module(erlmachine_supervisor_ct).

-export([boot/6]).
-export([install/4]).
-export([uninstall/4]).
-export([shutdown/4]).

-include("erlmachine_user.hrl").
-include("erlmachine_factory.hrl").
-include("erlmachine_assembly.hrl").
-include("erlmachine_system.hrl").
-include("erlmachine_transmission.hrl").

-spec boot(MN::model_no(), UID::uid(), GID::gid(), Specs::[spec()], Opt::list(), Env::list()) -> 
    success() | failure(term(), term()).
boot(MN, UID, GID, Specs, Opt, Env) ->
%% TODO: To provide test cases parametrization through Env;
    ct:log("~n~p:boot(~p, ~p, ~p, ~p, ~p, ~p)~n", [?MODULE, MN, UID, GID, Specs, Opt, Env]),
    erlmachine:success().

-spec install(MN::model_no(), UID::uid(), GID::gid(), Spec::spec()) -> 
    success() | failure(term(), term()).
install(MN, UID, GID, Spec) ->
    ct:log("~n~p:install(~p, ~p, ~p, ~p)~n", [?MODULE, MN, UID, GID, Spec]),
    erlmachine:success().

-spec uninstall(MN::model_no(), UID::uid(), GID::gid(), ID::term()) ->
    success() | failure(term(), term()).
uninstall(MN, UID, GID, ID) ->
    ct:log("~n~p:uninstall(~p, ~p, ~p, ~p)~n", [?MODULE, MN, UID, GID, ID]),
    erlmachine:success().

-spec shutdown(MN::model_no(), UID::uid(), GID::gid(), Reason::term()) ->
    success() | failure(term(), term()).
shutdown(MN, UID, GID, Reason) ->
    ct:log("~n~p:shutdown(~p, ~p, ~p, ~p)~n", [?MODULE, MN, UID, GID, Reason]),
    erlmachine:success().




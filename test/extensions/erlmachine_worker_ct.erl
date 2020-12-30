-module(erlmachine_worker_ct).

-export([boot/4]).
-export([process/3]).
-export([execute/3]).
-export([pressure/3]).
-export([shutdown/3]).

-include("erlmachine_user.hrl").
-include("erlmachine_factory.hrl").
-include("erlmachine_assembly.hrl").
-include("erlmachine_system.hrl").
-include("erlmachine_transmission.hrl").

-spec boot(UID::uid(), State::state(), Opt::list(), Env::map()) ->
                  success(state()) | failure(term(), term(), state()).
boot(UID, State, Opt, Env) ->
    %% TODO: To provide test cases parametrization through Env;
    ct:log("~n~p:boot(~p, ~p, ~p, ~p)~n", [?MODULE, UID, State, Opt, Env]),
    erlmachine:success(State).

-spec process(UID::uid(), Motion::term(), State::state()) ->
                     success(state()) | success(term(), state()) | failure(term(), term(), state()).
process(UID, Motion, State) ->
    ct:log("~n~p:process(~p, ~p, ~p)~n", [?MODULE, UID, Motion, State]),
    erlmachine:success(Motion, State).

-spec execute(UID::uid(), Command::term(), State::state()) ->
                     success(term(), state()) | failure(term(), term(), state()).
execute(UID, Command, State) ->
    ct:log("~n~p:execute(~p, ~p, ~p)~n", [?MODULE, UID, Command, State]),
    erlmachine:success(ok, State).

-spec pressure(UID::uid(), Load::term(), State::state()) ->
                      success(state()) | success(term(), state()) | failure(term(), term(), state()).
pressure(UID, Load, State) ->
    ct:log("~n~p:pressure(~p, ~p, ~p)~n", [?MODULE, UID, Load, State]),
    erlmachine:success(State).

-spec shutdown(UID::uid(), Reason::term(), State::state()) ->
                      success().
shutdown(UID, Reason, State) ->
    ct:log("~n~p:shutdown(~p, ~p, ~p)~n", [?MODULE, UID, State, Reason]),
    erlmachine:success().

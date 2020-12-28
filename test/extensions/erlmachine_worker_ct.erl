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

-spec boot(MN::model_no(), State::state(), Opt::list(), Env::map()) ->
                  success(state()) | failure(term(), term(), state()).
boot(MN, State, Opt, Env) ->
    %% TODO: To provide test cases parametrization through Env;
    ct:log("~n~p:boot(~p, ~p, ~p, ~p)~n", [?MODULE, MN, State, Opt, Env]),
    erlmachine:success(State).

-spec process(MN::model_no(), Motion::term(), State::state()) ->
                     success(state()) | success(term(), state()) | failure(term(), term(), state()).
process(MN, Motion, State) ->
    ct:log("~n~p:boot(~p, ~p, ~p)~n", [?MODULE, MN, Motion, State]),
    erlmachine:success(Motion, State).

-spec execute(MN::model_no(), Command::term(), State::state()) ->
                     success(term(), state()) | failure(term(), term(), state()).
execute(MN, Command, State) ->
    ct:log("~n~p:execute(~p, ~p, ~p)~n", [?MODULE, MN, Command, State]),
    erlmachine:success(ok, State).

-spec pressure(MN::model_no(), Load::term(), State::state()) ->
                      success(state()) | success(term(), state()) | failure(term(), term(), state()).
pressure(MN, Load, State) ->
    ct:log("~n~p:pressure(~p, ~p, ~p)~n", [?MODULE, MN, Load, State]),
    erlmachine:success(State).

-spec shutdown(MN::model_no(), Reason::term(), State::state()) ->
                      success().
shutdown(MN, Reason, State) ->
    ct:log("~n~p:shutdown(~p, ~p, ~p)~n", [?MODULE, MN, State, Reason]),
    erlmachine:success().

-module(erlmachine_model_sample).

-behaviour(erlmachine_worker_model).
%% Can be widely used as service versioning;
-vsn(['sample']).

-export([startup/4]).
-export([process/3]).
-export([execute/3]).
-export([pressure/3]).
-export([shutdown/3]).

-include_lib("erlbox/include/erlbox.hrl").

-include("erlmachine_user.hrl").

-type state() :: map().

-spec startup(UID::uid(), State::state(), Opt::map(), Env::map()) ->
                  success(state()) | failure(term(), term(), state()).
startup(UID, State, Opt, Env) ->
    %% TODO: To provide test cases parametrization through Env;
    io:format("~n~p:startup(~p, ~p, ~p, ~p)~n", [?MODULE, UID, State, Opt, Env]),
    erlmachine:success(State).

-spec process(UID::uid(), Motion::term(), State::state()) ->
                     success(state()) | success(term(), state()) | failure(term(), term(), state()).
process(UID, Motion, State) ->
    io:format("~n~p:process(~p, ~p, ~p)~n", [?MODULE, UID, Motion, State]),
    erlmachine:success(Motion, State).

-spec execute(UID::uid(), Action::term(), State::state()) ->
                     success(term(), state()) | failure(term(), term(), state()).
execute(UID, Action, State) ->
    io:format("~n~p:execute(~p, ~p, ~p)~n", [?MODULE, UID, Action, State]),
    erlmachine:success(ignore, State).

-spec pressure(UID::uid(), Load::term(), State::state()) ->
                      success(state()) | success(term(), state()) | failure(term(), term(), state()).
pressure(UID, Load, State) ->
    io:format("~n~p:pressure(~p, ~p, ~p)~n", [?MODULE, UID, Load, State]),
    erlmachine:success(State).

-spec shutdown(UID::uid(), Reason::term(), State::state()) ->
                      success().
shutdown(UID, Reason, State) ->
    io:format("~n~p:shutdown(~p, ~p, ~p)~n", [?MODULE, UID, State, Reason]),
    erlmachine:success().

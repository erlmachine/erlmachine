-module(erlmachine_worker_model).
%% NOTE: The main purpouse of the worker model is the ability to make impact on domain layer without affecting transport layer of service;
%% NOTE: Pressure callback was intentionally reduced to the "gear" mode with the idea to support readability of schemas (load balancing or routing facilities require to arrange additional component on a schema);

%% NOTE: Worker model concerns: domain layer processing;
%% API
-export([boot/1]).

-export([process/2]).
-export([execute/2]).
-export([pressure/2]).

-export([shutdown/2]).

-export([context/2]).

-type state() :: term().

-include("erlmachine_user.hrl").
-include("erlmachine_factory.hrl").
-include("erlmachine_assembly.hrl").
-include("erlmachine_system.hrl").

-callback boot(UID::uid(), State::state(), Opt::[term()], Env::map()) ->
    success(state()) | failure(term(), term(), state()).

-callback process(UID::uid(), Motion::term(), State::state()) ->
    success(state()) | success(term(), state()) | failure(term(), term(), state()).

-callback process(UID::uid(), Motion::term(), Socket::term(), Range::[term()], State::state()) ->
    success(state()) | success(term(), state()) | failure(term(), term(), state()).

-callback execute(UID::uid(), Command::term(), State::state()) ->
    success(term(), state()) | failure(term(), term(), state()).

-callback pressure(UID::uid(), Load::term(), State::state()) ->
    success(state()) | success(term(), state()) | failure(term(), term(), state()).

-callback shutdown(UID::uid(), Reason::term(), State::state()) ->
    success().

%% NOTE: Lazy callbacks are designed to reduce computational resources when extension is deadlocked;
-optional_callbacks([process/3, process/5, pressure/3, shutdown/3]).

-spec boot(Context::assembly()) ->
                   success(assembly()) | failure(term(), term(), assembly()).
boot(Context) ->
    UID = erlmachine_assembly:uid(Context),

    Model = erlmachine_assembly:model(Context), Name = erlmachine_model:name(Model),
    Opt = erlmachine_model:options(Model),
    Body = erlmachine_assembly:body(Context),
    Env = erlmachine_assembly:env(Context),

    Res = Name:boot(UID, Body, Opt, Env),
    context(Res, Context).


-spec process(Context::assembly(), Motion::term()) ->
                    success(assembly()) | failure(term(), term(), assembly()).
process(Context, Motion) ->
    Res = erlmachine_worker_process:process(Context, Motion),
    trim(Res).

-spec execute(Context::assembly(), Command::term()) ->
                      success(term(), assembly()) | failure(term(), term(), assembly()).
execute(Context, Command) ->
    UID = erlmachine_assembly:uid(Context),

    Model = erlmachine_assembly:model(Context), Name = erlmachine_model:name(Model),
    Body = erlmachine_assembly:body(Context),
    Res = Name:execute(UID, Command, Body),
    context(Res, Context).

-spec pressure(Context::assembly(), Load::term()) ->
                      success(assembly()) | failure(term(), term(), assembly()).
pressure(Context, Load) ->
    Res = erlmachine_worker_pressure:pressure(Context, Load),
    trim(Res).

-spec shutdown(Context::assembly(), Reason::term()) -> 
                      success().
shutdown(Context, Reason) ->
    UID = erlmachine_assembly:uid(Context),

    Model = erlmachine_assembly:model(Context), Name = erlmachine_model:name(Model),
    Body = erlmachine_assembly:body(Context),

    Mod = Name, Fun = shutdown, Args = [UID, Reason, Body],
    Def = erlmachine:success(),
    erlmachine:optional_callback(Mod, Fun, Args, Def).

%% NOTE: This function is responsible to control model output;
%% TODO: To provide errors debug at this level;
context({ok, Body}, Context) ->
    Rel = erlmachine_assembly:body(Context, Body),
    erlmachine:success(Rel);
context({ok, Res, Body}, Context) ->
    Rel = erlmachine_assembly:body(Context, Body),
    erlmachine:success(Res, Rel);
context({error, {E, R}, Body}, Context) ->
    Rel = erlmachine_assembly:body(Context, Body),
    erlmachine:failure(E, R, Rel).

trim({ok, _, Context}) ->
    erlmachine:success(Context);
trim(Res) ->
    Res.


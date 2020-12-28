-module(erlmachine_worker).
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

-include("erlmachine_factory.hrl").
-include("erlmachine_assembly.hrl").
-include("erlmachine_system.hrl").

-callback boot(MN::model_no(), State::state(), Opt::list(), Env::map()) ->
    success(state()) | failure(term(), term(), state()).

-callback process(MN::model_no(), Motion::term(), State::state()) ->
    success(state()) | success(term(), state()) | failure(term(), term(), state()).

-callback process(MN::model_no(), Motion::term(), Socket::term(), Range::[model_no()], State::state()) ->
    success(state()) | success(term(), state()) | failure(term(), term(), state()).

-callback execute(MN::model_no(), Command::term(), State::state()) ->
    success(term(), state()) | failure(term(), term(), state()).

-callback pressure(MN::model_no(), Load::term(), State::state()) ->
    success(state()) | success(term(), state()) | failure(term(), term(), state()).

-callback shutdown(MN::model_no(), Reason::term(), State::state()) ->
    success().

%% NOTE: Lazy callbacks are designed to reduce computational resources when extension is deadlocked;
-optional_callbacks([process/3, process/5, pressure/3, shutdown/3]).

-spec boot(Context::assembly()) ->
                   success(assembly()) | failure(term(), term(), assembly()).
boot(Context) ->
    MN = erlmachine_assembly:model_no(Context),

    Model = erlmachine_assembly:model(Context), Name = erlmachine_model:name(Model),
    Opt = erlmachine_model:options(Model),
    Body = erlmachine_assembly:body(Context),
    Env = erlmachine_assembly:env(Context),

    Res = Name:boot(MN, Body, Opt, Env),
    context(Res, Context).


-spec process(Context::assembly(), Motion::term()) ->
                    success(assembly()) | success(term(), assembly()) | failure(term(), term(), assembly()).
process(Context, Motion) ->
    erlmachine_process:process(Context, Motion).

-spec execute(Context::assembly(), Command::term()) ->
                      success(term(), assembly()) | failure(term(), term(), assembly()).
execute(Context, Command) ->
    MN = erlmachine_assembly:model_no(Context),

    Model = erlmachine_assembly:model(Context), Name = erlmachine_model:name(Model),
    Body = erlmachine_assembly:body(Context),
    Res = Name:execute(MN, Command, Body),
    context(Res, Context).

-spec pressure(Context::assembly(), Load::term()) ->
                      success(assembly()) | success(term(), assembly()) | failure(term(), term(), assembly()).
pressure(Context, Load) ->
    erlmachine_pressure:pressure(Context, Load).

-spec shutdown(Context::assembly(), Reason::term()) -> 
                      success().
shutdown(Context, Reason) ->
    MN = erlmachine_assembly:model_no(Context),

    Model = erlmachine_assembly:model(Context), Name = erlmachine_model:name(Model),
    Body = erlmachine_assembly:body(Context),

    Mod = Name, Fun = shutdown, Args = [MN, Reason, Body],
    Def = erlmachine:success(),
    erlmachine:optional_callback(Mod, Fun, Args, Def).

%% NOTE: This function is responsible to control model output;
%% TODO: To provide errors debug at this level;
context({ok, Body}, Context) ->
    Rel = erlmachine_assembly:body(Context, Body),
    {ok, Rel};
context({ok, Res, Body}, Context) ->
    Rel = erlmachine_assembly:body(Context, Body),
    {ok, Res, Rel};
context({error, {E, R}, Body}, Context) ->
    Rel = erlmachine_assembly:body(Context, Body),
    {error, {E, R}, Rel}.

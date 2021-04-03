-module(erlmachine_worker_model).
%% NOTE: The main purpouse of the worker model is the ability to make impact on domain layer without affecting transport layer of service;
%% NOTE: Pressure callback was intentionally reduced to the "gear" mode with the idea to support readability of schemas (load balancing or routing facilities require to arrange additional component on a schema);

%% NOTE: Worker model concerns: domain layer processing;
%% API

-export([is_worker_model/1]).

-export([startup/1]).

-export([process/2]).
-export([execute/2]).
-export([pressure/2]).

-export([shutdown/2]).

-export([body/2]).

-type state() :: term().

-include("erlmachine_user.hrl").
-include("erlmachine_assembly.hrl").
-include("erlmachine_graph.hrl").
-include("erlmachine_system.hrl").

-callback startup(UID::uid(), State::state(), Opt::[term()], Env::map()) ->
    success(state()) | failure(term(), term(), state()).

-callback process(UID::uid(), Motion::term(), State::state()) ->
    success(state()) | success(term(), state()) | failure(term(), term(), state()).

-callback process(UID::uid(), Motion::term(), Port::term(), Scheduled::[term()], State::state()) ->
    success(state()) | success(term(), state()) | failure(term(), term(), state()).

-callback execute(UID::uid(), Command::term(), State::state()) ->
    success(term(), state()) | failure(term(), term(), state()).

-callback pressure(UID::uid(), Load::term(), State::state()) ->
    success(state()) | success(term(), state()) | failure(term(), term(), state()).

-callback shutdown(UID::uid(), Reason::term(), State::state()) ->
    success().

%% NOTE: Lazy callbacks are designed to reduce computational resources when extension is deadlocked;
-optional_callbacks([process/3, process/5, pressure/3, shutdown/3]).

-spec is_worker_model(Module::atom()) -> boolean().
is_worker_model(Module) ->
    lists:member(?MODULE, erlmachine:behaviours(Module)).

%%%  Transmission API

-spec startup(Assembly::assembly()) ->
                   success(assembly()) | failure(term(), term(), assembly()).
startup(Assembly) ->
    Model = erlmachine_assembly:model(Assembly), Module = erlmachine_model:module(Model),

    UID = erlmachine_assembly:uid(Assembly),
    Body = erlmachine_assembly:body(Assembly),
    Opt = erlmachine_model:options(Model),
    Env = erlmachine_assembly:env(Assembly),

    Res = Module:startup(UID, Body, Opt, Env), body(Res, Assembly).

-spec process(Assembly::assembly(), Motion::term()) ->
                    success(assembly()) | failure(term(), term(), assembly()).
process(Assembly, Motion) ->
    Res = erlmachine_worker_process:process(Assembly, Motion),
    trim(Res).

-spec execute(Assembly::assembly(), Command::term()) ->
                      success(term(), assembly()) | failure(term(), term(), assembly()).
execute(Assembly, Command) ->
    Model = erlmachine_assembly:model(Assembly), Module = erlmachine_model:module(Model),

    UID = erlmachine_assembly:uid(Assembly),
    Body = erlmachine_assembly:body(Assembly),

    Res = Module:execute(UID, Command, Body), body(Res, Assembly).

-spec pressure(Context::assembly(), Load::term()) ->
                      success(assembly()) | failure(term(), term(), assembly()).
pressure(Context, Load) ->
    Res = erlmachine_worker_pressure:pressure(Context, Load),
    trim(Res).

-spec shutdown(Context::assembly(), Reason::term()) ->
                      success().
shutdown(Context, Reason) ->
    Model = erlmachine_assembly:model(Context), Module = erlmachine_model:module(Model),

    UID = erlmachine_assembly:uid(Context),
    Body = erlmachine_assembly:body(Context),
    Fun = shutdown, Args = [UID, Reason, Body], Def = erlmachine:success(),

    erlmachine:optional_callback(Module, Fun, Args, Def).

%% NOTE: This function is responsible to control model output;
%% TODO: To provide errors debug at this level;
body({ok, Body}, Assembly) ->
    Rel = erlmachine_assembly:body(Assembly, Body),
    erlmachine:success(Rel);
body({ok, Res, Body}, Assembly) ->
    Rel = erlmachine_assembly:body(Assembly, Body),
    erlmachine:success(Res, Rel);
body({error, {E, R}, Body}, Assembly) ->
    Rel = erlmachine_assembly:body(Assembly, Body),
    erlmachine:failure(E, R, Rel).

trim({ok, _, Assembly}) ->
    erlmachine:success(Assembly);
trim(Res) ->
    Res.

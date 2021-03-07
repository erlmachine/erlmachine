-module(erlmachine_worker_pressure).

-export([pressure/2]).
-export([pass/2]).

-include("erlmachine_factory.hrl").
-include("erlmachine_assembly.hrl").
-include("erlmachine_system.hrl").

-spec pressure(Context::assembly(), Load::term()) ->
                      success(assembly()) | success(term(), assembly()) | failure(term(), term(), assembly()).
pressure(Context, Load) ->
    erlmachine_transmission:pass(?MODULE, Context, Load).

%%%  transmission callbacks

-spec pass(Context::assembly(), Motion::term()) ->
                    success(assembly()) | success(term(), assembly()) | failure(term(), term(), assembly()).
pass(Context, Motion) ->
    UID = erlmachine_assembly:uid(Context),

    Model = erlmachine_assembly:model(Context), Name = erlmachine_model:name(Model),
    Body = erlmachine_assembly:body(Context),

    Mod = Name, Fun = 'pressure', Args = [UID, Motion, Body],
    Def = erlmachine:success(Motion, Context),
    Res = erlmachine:optional_callback(Mod, Fun, Args, Def), 
    erlmachine_worker_model:context(Res, Context).

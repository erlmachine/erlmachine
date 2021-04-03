-module(erlmachine_worker_pressure).

-export([pressure/2]).
-export([pass/2]).

-include("erlmachine_factory.hrl").
-include("erlmachine_assembly.hrl").
-include("erlmachine_system.hrl").

-spec pressure(Assembly::assembly(), Load::term()) ->
                      success(assembly()) | success(term(), assembly()) | failure(term(), term(), assembly()).
pressure(Assembly, Load) ->
    erlmachine_transmission:pass(?MODULE, Assembly, Load).

%%%  transmission callbacks

-spec pass(Assembly::assembly(), Motion::term()) ->
                    success(assembly()) | success(term(), assembly()) | failure(term(), term(), assembly()).
pass(Assembly, Motion) ->
    Model = erlmachine_assembly:model(Assembly), Name = erlmachine_model:name(Model),

    UID = erlmachine_assembly:uid(Assembly),
    Body = erlmachine_assembly:body(Assembly),
    Mod = Name, Fun = 'pressure', Args = [UID, Motion, Body], Def = erlmachine:success(Motion, Assembly),

    Res = erlmachine:optional_callback(Mod, Fun, Args, Def), 
    erlmachine_worker_model:body(Res, Assembly).

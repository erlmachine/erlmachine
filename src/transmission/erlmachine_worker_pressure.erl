-module(erlmachine_worker_pressure).

-export([pressure/2]).
-export([pass/2]).

-include_lib("erlbox/include/erlbox.hrl").

-include("erlmachine_factory.hrl").
-include("erlmachine_assembly.hrl").

-spec pressure(Assembly::assembly(), Load::term()) ->
                      success(assembly()) | success(term(), assembly()) | failure(term(), term(), assembly()).
pressure(Assembly, Load) ->
    erlmachine_transmission:pass(?MODULE, Assembly, Load).

%%%  transmission callbacks

-spec pass(Assembly::assembly(), Motion::term()) ->
                    success(assembly()) | success(term(), assembly()) | failure(term(), term(), assembly()).
pass(Assembly, Motion) ->
    Model = erlmachine_assembly:model(Assembly), Module = erlmachine_model:module(Model),

    UID = erlmachine_assembly:uid(Assembly),
    Body = erlmachine_assembly:body(Assembly),
    Fun = 'pressure', Args = [UID, Motion, Body], Def = erlmachine:success(Motion, Assembly),

    Res = erlmachine:optional_callback(Module, Fun, Args, Def),
    erlmachine_worker_model:body(Res, Assembly).

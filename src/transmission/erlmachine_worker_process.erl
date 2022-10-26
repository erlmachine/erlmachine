-module(erlmachine_worker_process).

-export([process/2]).
-export([mesh/4, pass/2]).

-include_lib("erlbox/include/erlbox.hrl").

-include("erlmachine_factory.hrl").
-include("erlmachine_assembly.hrl").

-spec process(Assembly::assembly(), Motion::term()) ->
                     success(assembly()) | success(term(), assembly()) | failure(term(), term(), assembly()).
process(Assembly, Motion) ->
    Model = erlmachine_assembly:model(Assembly), Module = erlmachine_model:module(Model),
    %% NOTE: Shaft mode callback;
    Exported = erlang:function_exported(Module, 'process', 5),
    if Exported ->
            erlmachine_transmission:mesh(?MODULE, Assembly, Motion);
       true ->
            erlmachine_transmission:pass(?MODULE, Assembly, Motion)
    end.

%%%  transmission callbacks

-spec mesh(Assembly::assembly(), Motion::term(), Ext::assembly(), T::[assembly()]) ->
                    success(assembly()) | success(term(), assembly()) | failure(term(), term(), assembly()).
mesh(Assembly, Motion, Ext, T) ->
    Model = erlmachine_assembly:model(Assembly), Module = erlmachine_model:module(Model),

    UID = erlmachine_assembly:uid(Assembly),

    Body = erlmachine_assembly:body(Assembly),
    Port = erlmachine_assembly:port(Ext),
    Scheduled = [erlmachine_assembly:port(E) || E <- T],

    Res = Module:process(UID, Motion, Port, Scheduled, Body),
    erlmachine_worker_model:body(Res, Assembly).

-spec pass(Assembly::assembly(), Motion::term()) ->
                  success(assembly()) | success(term(), assembly()) | failure(term(), term(), assembly()).
pass(Assembly, Motion) ->
    Model = erlmachine_assembly:model(Assembly), Module = erlmachine_model:module(Model),

    UID = erlmachine_assembly:uid(Assembly),
    Body = erlmachine_assembly:body(Assembly),
    Fun = process, Args = [UID, Motion, Body], Def = erlmachine:success(Motion, Assembly),

    Res = erlmachine:optional_callback(Module, Fun, Args, Def),
    erlmachine_worker_model:body(Res, Assembly).

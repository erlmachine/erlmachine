-module(erlmachine_process).

-export([process/2]).
-export([mesh/4, pass/2]).

-include("erlmachine_factory.hrl").
-include("erlmachine_assembly.hrl").
-include("erlmachine_system.hrl").

-spec process(Context::assembly(), Motion::term()) ->
                     success(assembly()) | success(term(), assembly()) | failure(term(), term(), assembly()).
process(Context, Motion) ->
    Model = erlmachine_assembly:model(Context), Name = erlmachine_model:name(Model),
    %% NOTE: Shaft mode callback;
    Exported = erlang:function_exported(Name, 'rotate', 5),
    if Exported ->
            erlmachine_transmission:mesh(?MODULE, Context, Motion);
       true ->
            erlmachine_transmission:pass(?MODULE, Context, Motion)
    end.

%%%===================================================================
%%%  transmission callbacks
%%%===================================================================

-spec mesh(Context::assembly(), Motion::term(), Ext::assembly(), Range::[assembly()]) ->
                    success(assembly()) | success(term(), assembly()) | failure(term(), term(), assembly()).
mesh(Context, Motion, Ext, Range) ->
    UID = erlmachine_assembly:uid(Context),

    Model = erlmachine_assembly:model(Context), Name = erlmachine_model:name(Model),

    Body = erlmachine_assembly:body(Context),
    Socket = erlmachine_assembly:socket(Ext),
    MNs = [erlmachine_assembly:model_no(E) || E <- Range],

    Res = Name:process(UID, Motion, Socket, MNs, Body),
    erlmachine_worker:context(Res, Context).

-spec pass(Context::assembly(), Motion::term()) ->
                  success(assembly()) | success(term(), assembly()) | failure(term(), term(), assembly()).
pass(Context, Motion) ->
    UID = erlmachine_assembly:uid(Context),

    Model = erlmachine_assembly:model(Context), Name = erlmachine_model:name(Model),
    Body = erlmachine_assembly:body(Context),

    Mod = Name, Fun = process, Args = [UID, Motion, Body],
    Def = erlmachine:success(Motion, Context),
    Res = erlmachine:optional_callback(Mod, Fun, Args, Def), 
    erlmachine_worker:context(Res, Context).

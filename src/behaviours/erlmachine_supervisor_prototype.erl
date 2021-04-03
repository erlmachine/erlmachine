-module(erlmachine_supervisor_prototype).
%% NOTE: The main puprouse of supervisor prototype is the ability to change monitoring layer without affecting credentials layer of an extension

%% NOTE: There are few examples of monitorings implementations:

%% 1. erlang:monitor/2
%% 2. supervisor2
%% 3. mirrored_supervisor

%% NOTE: There is should exists the "mother" or bootloader extension which will load and initiate transmission
%% NOTE: Supervisor prototype concerns: health check, recovery management
%% NOTE: In comparison to worker prototype a supervisor prototype is state-less

%% TODO:
%% a) To simplify context for network transmission
%% b) To gather statistics into graph

%% TODO https://github.com/rabbitmq/rabbitmq-common/blob/master/src/supervisor2.erl

%% API

-export([is_supervisor_prototype/1]).

-export([startup/2]).
-export([install/2, uninstall/2]).

%% Context API
-export([init/1, start_child/1, terminate_child/1]).

-type context() :: term().

-include("erlmachine_factory.hrl").
-include("erlmachine_assembly.hrl").
-include("erlmachine_graph.hrl").
-include("erlmachine_system.hrl").

-callback prototype_init(SN::serial_no(), Specs::[map()], Context::context(), Opt::[term()]) ->
    success(pid()) | failure(term(), term()).

-callback prototype_start_child(SN::serial_no(), Spec::map(), Context::context()) ->
    success(pid()) | failure(term(), term()).

-callback prototype_terminate_child(SN::serial_no(), ID::term(), Context::context()) ->
    success().

-spec is_supervisor_prototype(Module::atom()) -> boolean().
is_supervisor_prototype(Module) ->
    lists:member(?MODULE, erlmachine:behaviours(Module)).

%%%  Transmission API

-spec startup(Assembly::assembly(), Exts::[assembly()]) ->
                     success(pid()) | failure(term(), term()).
startup(Assembly, Exts) ->
    SN = erlmachine_assembly:serial_no(Assembly),

    Prot = erlmachine_assembly:prototype(Assembly),
    Module = erlmachine_prototype:module(Prot), Opt = erlmachine_prototype:options(Prot),
    Specs = [erlmachine_transmission:spec(Ext)|| Ext <- Exts],

    Module:prototype_init(SN, Specs, _Context = [Assembly, Exts], Opt).

-spec install(Assembly::assembly(), Ext::assembly()) ->
                     success(pid()) | failure(term(), term()).
install(Assembly, Ext) ->
    SN = erlmachine_assembly:serial_no(Assembly),

    Spec = erlmachine_transmission:spec(Ext),

    Prot = erlmachine_assembly:prototype(Assembly),
    Module = erlmachine_prototype:module(Prot),

    Module:prototype_start_child(SN, Spec, _Context = [Assembly, Ext]).

-spec uninstall(Assembly::assembly(), V::vertex()) ->
                       failure(term(), term()).
uninstall(Assembly, V) ->
    SN = erlmachine_assembly:serial_no(Assembly),

    Prot = erlmachine_assembly:prototype(Assembly),
    Module = erlmachine_prototype:module(Prot),

    Module:prototype_terminate_child(SN, V, _Context = [Assembly, V]).

%%% Prototype API

-spec init(Context::context()) ->
                  success() | failure(term(), term()).
init(Context) ->
    [Assembly, Exts] = Context,
    erlmachine_supervisor_model:startup(Assembly, Exts).

-spec start_child(Context::context()) ->
                         success() | failure(term(), term()).
start_child(Context) ->
    [Assembly, Ext] = Context,
    erlmachine_supervisor_model:install(Assembly, Ext).

-spec terminate_child(Context::context()) ->
                             success().
terminate_child(Context) ->
    [Assembly, V] = Context,
    erlmachine_supervisor_model:uninstall(Assembly, V).

-module(erlmachine_supervisor_prototype).
%% NOTE: The main puprouse of supervisor prototype is the ability to change monitoring layer without affecting credentials layer of service

%% NOTE: There are few examples:

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
-export([install/3, uninstall/2]).

%% Context API
-export([init/2, start_child/2, terminate_child/2]).

-type context() :: term().

-type spec() :: erlmachine_transmission:spec().

-include("erlmachine_factory.hrl").
-include("erlmachine_assembly.hrl").
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

%% NOTE: There is responsibility of decorated module to provide the right entry on graph;
-spec startup(Assembly::assembly(), Exts::[assembly()]) ->
                     success(pid()) | failure(term(), term()).
startup(Assembly, Exts) ->
    SN = erlmachine_assembly:serial_no(Assembly),

    Prot = erlmachine_assembly:prototype(Assembly),
    Name = erlmachine_prototype:name(Prot), Opt = erlmachine_prototype:options(Prot),
    Specs = [erlmachine_transmission:spec(Ext)|| Ext <- Exts],

    Name:prototype_init(SN, Specs, _Context = [Assembly, Exts], Opt).

-spec install(Assembly::assembly(), Ext::assembly(), Env::map()) ->
                     success(pid()) | failure(term(), term()).
install(Assembly, Ext, Env) ->
    SN = erlmachine_assembly:serial_no(Assembly),

    Spec = erlmachine_transmission:spec(Ext, Env),

    Prot = erlmachine_assembly:prototype(Assembly),
    Name = erlmachine_prototype:name(Prot),

    Name:prototype_start_child(SN, Spec, _Context = Assembly).

-spec uninstall(Assembly::assembly(), ID::term()) ->
                       failure(term(), term()).
uninstall(Assembly, ID) ->
    SN = erlmachine_assembly:serial_no(Assembly),

    Prot = erlmachine_assembly:prototype(Assembly),
    Name = erlmachine_prototype:name(Prot),

    Name:prototype_terminate_child(SN, ID, _Context = Assembly).

%%% Prototype API

-spec init(Context::context()) ->
                  success() | failure(term(), term()).
init(Context) ->
    [Assembly, Exts] = Context,
    Vs = [erlmachine_assembly:vertex(Ext)|| Ext <- Exts],

    erlmachine_supervisor_model:startup(Context, Vs).

-spec start_child(Context::context(), Spec::[map()]) ->
                         success() | failure(term(), term()).
start_child(Context, Spec) ->
    erlmachine_supervisor_model:install(Context, Spec).

-spec terminate_child(Context::context(), ID::term()) ->
                             success().
terminate_child(Context, ID) ->
    erlmachine_supervisor_model:uninstall(Context, ID).

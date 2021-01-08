-module(erlmachine_supervisor_prototype).
%% NOTE: The main puprouse of the supervisor is the ability to change monitor layer without affecting business layer of service;
%% NOTE: Potential cases:
%% 1. erlang:monitor/2;
%% 2. supervisor2;
%% 3. mirrored_supervisor;

%% NOTE: Supervisor prototype concerns: health check, recovery management;
%% NOTE: In comparison to worker prototype a supervisor prototype is state-less;

%% TODO:
%% a) To simplify context for network transmission;
%% b) To gather statistics into graph;

%% TODO https://github.com/rabbitmq/rabbitmq-common/blob/master/src/supervisor2.erl

%% API
-export([boot/2]).

-export([install/2, uninstall/2]).

-export([shutdown/3]).

%% Context API
-export([init/2, start_child/2, terminate_child/2, terminate/2]).

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

-callback prototype_terminate(SN::serial_no(), Reason::term(), Timeout::term(), Context::context()) ->
    success().

%%%===================================================================
%%%  Transmission API
%%%===================================================================
%% NOTE: There is responsibility of decorated module to provide the right entry on graph;
-spec boot(Assembly::assembly(), Exts::[assembly()]) ->
                     success(pid()) | failure(term(), term()).
boot(Assembly, Exts) ->
    SN = erlmachine_assembly:serial_no(Assembly),

    Prot = erlmachine_assembly:prototype(Assembly),
    Name = erlmachine_prototype:name(Prot), Opt = erlmachine_prototype:options(Prot),
    Specs = [spec(Assembly, Ext)|| Ext <- Exts],

    Name:prototype_init(SN, Specs, _Context = Assembly, Opt).

-spec spec(Assembly::assembly(), Ext::assembly()) -> spec().
spec(Assembly, Ext) ->
    Schema = erlmachine_assembly:schema(Assembly),
    Env = erlmachine_assembly:env(Assembly),
    Rel = erlmachine_assembly:schema(erlmachine_assembly:env(Ext, Env), Schema),
    erlmachine_transmission:spec(Rel).

-spec install(Assembly::assembly(), Ext::assembly()) ->
                     success(pid()) | failure(term(), term()).
install(Assembly, Ext) ->
    SN = erlmachine_assembly:serial_no(Assembly),

    Spec = spec(Assembly, Ext),

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

-spec shutdown(Assembly::assembly(), Reason::term(), Timeout::term()) ->
                       success().
shutdown(Assembly, Reason, Timeout) ->
    SN = erlmachine_assembly:serial_no(Assembly),

    Prot = erlmachine_assembly:prototype(Assembly),
    Name = erlmachine_prototype:name(Prot),

    Name:prototype_terminate(SN, Reason, Timeout, _Context = Assembly).

%%%===================================================================
%%% Prototype API
%%%===================================================================

-spec init(Context::context(), Specs::[map()]) ->
                  success() | failure(term(), term()).
init(Context, Specs) ->
    erlmachine_supervisor_model:boot(Context, Specs).

-spec start_child(Context::context(), Spec::[map()]) ->
                         success() | failure(term(), term()).
start_child(Context, Spec) ->
    erlmachine_supervisor_model:install(Context, Spec).

-spec terminate_child(Context::context(), ID::term()) ->
                             success().
terminate_child(Context, ID) ->
    erlmachine_supervisor_model:uninstall(Context, ID).

-spec terminate(Context::context(), Reason::term()) ->
                       success().
terminate(Context, Reason) ->
    erlmachine_supervisor_model:shutdown(Context, Reason).

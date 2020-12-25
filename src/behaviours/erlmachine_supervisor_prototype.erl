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

%% API
-export([boot/1]).

-export([install/2, uninstall/2]).

-export([shutdown/2]).

%% Context API
-export([init/2, start_child/2, terminate_child/2, terminate/1]).

-type context() :: term().

-include("erlmachine_factory.hrl").
-include("erlmachine_assembly.hrl").
-include("erlmachine_system.hrl").

-callback prototype_init(SN::serial_no(), Specs::list(map()), Context::context(), Opt::list()) ->
    success(pid()) | failure(term(), term()).

-callback prototype_start_child(SN::serial_no(), Spec::map(), Context::context()) ->
    success(pid()) | failure(term(), term()).

-callback prototype_terminate_child(SN::serial_no(), ID::term(), Context::context()) ->
    success().

-callback prototype_terminate(SN::serial_no(), Context::context()) ->
    success().

%%%===================================================================
%%%  Transmission API
%%%===================================================================

-spec boot(Assembly::assembly()) ->
                     success(pid()) | failure(term(), term()).
boot(Assembly) ->
    SN = erlmachine_assembly:serial_no(Assembly),

    Exts = erlmachine_assembly:extensions(Assembly),
    Specs = [erlmachine_transmission:spec(Assembly, Ext)|| Ext <- Exts],

    Prot = erlmachine_assembly:prototype(Assembly),
    Name = erlmachine_prototype:name(Prot), Opt = erlmachine_prototype:options(Prot),

    Name:prototype_init(SN, Specs, _Context = Assembly, Opt).

-spec install(Assembly::assembly(), Ext::assembly()) ->
                     success(pid()) | failure(term(), term()).
install(Assembly, Ext) ->
    SN = erlmachine_assembly:serial_no(Assembly),

    Spec = erlmachine_transmission:spec(Assembly, Ext),

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

-spec shutdown(Assembly::assembly(), Reason::term()) ->
                       success().
shutdown(Assembly, Reason) ->
    SN = erlmachine_assembly:serial_no(Assembly),

    Prot = erlmachine_assembly:prototype(Assembly),
    Name = erlmachine_prototype:name(Prot),

    Name:prototype_terminate(SN, Reason, _Context = Assembly).

%%%===================================================================
%%% Prototype API
%%%===================================================================

-spec init(Context::context(), Specs::[map()]) ->
                  success() | failure(term(), term()).
init(Context, Specs) ->
    erlmachine_supervisor:boot(Context, Specs).

-spec start_child(Context::context(), Spec::[map()]) ->
                         success() | failure(term(), term()).
start_child(Context, Spec) ->
    erlmachine_supervisor:install(Context, Spec).

-spec terminate_child(Context::context(), ID::term()) ->
                             success().
terminate_child(Context, ID) ->
    erlmachine_supervisor:uninstall(Context, ID).

-spec terminate(Context::context()) ->
                       success().
terminate(Context) ->
    erlmachine_supervisor:shutdown(Context).

%% TODO https://github.com/rabbitmq/rabbitmq-common/blob/master/src/supervisor2.erl



-module(erlmachine_supervisor_prototype).
%% NOTE: The main puprouse of the supervisor is the ability to change monitor layer without affecting business layer of service;
%% NOTE: There are few examples:
%% 1. erlang:monitor/2;
%% 2. supervisor2;
%% 3. mirrored_supervisor;

%% NOTE: Supervisor prototype concerns: health check, recovery management;
%% NOTE: In comparison to erlmachine_worker_prototype a supervisor prototype is stateless;

%% TODO:
%% a) To simplify context for network transmission;
%% b) To gather statistics into graph;

%% API
-export([start/1]).
-export([install/2, uninstall/2]).
-export([stop/1]).

%% Context API
-export([init/1, start_child/1, terminate_child/1, terminate/1]).

-type context() :: term().

-include("erlmachine_factory.hrl").
-include("erlmachine_assembly.hrl").
-include("erlmachine_system.hrl").

-callback prototype_init(SN::serial_no(), Specs::list(map()), Context::context(), Opts::list()) ->
    success(pid()) | failure(term(), term()).

-callback prototype_start_child(SN::serial_no(), Spec::map(), Context::context()) ->
    success(pid()) | failure(term(), term()).

-callback prototype_terminate_child(SN::serial_no(), Id::term(), Context::context()) ->
    success().

-callback prototype_terminate(SN::serial_no(), Context::context()) ->
    success().

%%%===================================================================
%%%  Transmission API
%%%===================================================================

-spec start(Assembly::assembly()) ->
                     success(pid()) | failure(term(), term()).
start(Assembly) ->
    SN = erlmachine_assembly:serial_no(Assembly),
    Model = erlmachine_assembly:model(Assembly), Prot = erlmachine_model:prototype(Model),
    Exts = [Ext|| Ext <- erlmachine_assembly:extensions(Assembly)],
    Specs = [spec(Assembly, Ext)|| Ext <- Exts],
    Opts = erlmachine_prototype:options(Prot),
    Name = erlmachine_prototype:name(Prot),
    Name:prototype_init(SN, Specs, [Assembly, Exts], Opts).

-spec install(Assembly::assembly(), Ext::assembly()) ->
                     success(pid()) | failure(term(), term()).
install(Assembly, Ext) ->
    SN = erlmachine_assembly:serial_no(Assembly),
    Model = erlmachine_assembly:model(Assembly), Prot = erlmachine_model:prototype(Model),
    Spec = spec(Assembly, Ext),
    Name = erlmachine_prototype:name(Prot),
    Name:prototype_start_child(SN, Spec, [Assembly, Ext]).

-spec uninstall(Assembly::assembly(), Id::term()) ->
                       failure(term(), term()).
uninstall(Assembly, Id) ->
    SN = erlmachine_assembly:serial_no(Assembly),
    Model = erlmachine_assembly:model(Assembly), Prot = erlmachine_model:prototype(Model),
    Name = erlmachine_prototype:name(Prot),
    Name:prototype_terminate_child(SN, Id, _Context = [Assembly, Id]).

-spec stop(Assembly::assembly()) ->
                       success().
stop(Assembly) ->
    SN = erlmachine_assembly:serial_no(Assembly),
    Model = erlmachine_assembly:model(Assembly), Prot = erlmachine_model:prototype(Model),
    Name = erlmachine_prototype:name(Prot),
    Name:prototype_terminate(SN, Assembly).

%% TODO erlmachine_supervisor:install() will be called from prototype;
-spec spec(Assembly::assembly(), Ext::assembly()) -> Spec::map().
spec(Assembly, Ext) ->
    SN = erlmachine_assembly:serial_no(Assembly),
    Schema = erlmachine_assembly:schema(Assembly),
    Env = erlmachine_assembly:env(Assembly),
    Rel = erlmachine_assembly:schema(erlmachine_assembly:env(Ext, Env), Schema),
    Module = erlmachine_assembly:name(Rel),
    %% TODO Start arguments need to be formed outside;
    Start = {Module, start, [Rel]},

    Type = Module:type(), true = (Type == supervisor orelse Type == worker),
    #{
      id => SN,
      start => Start,
      type => Type
     }.

%%%===================================================================
%%% Prototype API
%%%===================================================================

-spec init(Context::context()) -> 
                  success() | failure(term(), term()).
init(Context) ->
    [_Assembly, _Exts] = Context,
    erlmachine:success().

-spec start_child(Context::context()) -> 
                         success() | failure(term(), term()).
start_child(Context) ->
    [_Assembly, _Ext] = Context,
    erlmachine:success().

-spec terminate_child(Context::context()) -> 
                             success().
terminate_child(Context) ->
    [_Assembly, _Id] = Context,
    erlmachine:success().

-spec terminate(Context::context()) ->
                       success().
terminate(Context) ->
    _Assembly = Context,
    erlmachine:success().

%% TODO https://github.com/rabbitmq/rabbitmq-common/blob/master/src/supervisor2.erl



-module(erlmachine_supervisor_prototype).
%% The main purpouse of the supervisour prototype is to support different supervisor implementations without affecting tracing/business layer of application;
-export([init/2, start_child/3, terminate/1, terminate_child/2]).

-export([ack/1]).

-include("erlmachine_factory.hrl").
-include("erlmachine_system.hrl").

%% install(Assembly::assembly(), Exts::list(assembly()));
%% erlmachine_supervisor_prototype:init(Args);
-callback prototype_init(SN::term(), Context::assembly(), Specs::list(map()), Ids::list(), Opt::list()) -> 
    success(pid()) | failure(term(), term()).

-callback prototype_start_child(SN::term(), Context::assembly(), Spec::map(), Id::term()) ->
    success(pid()) | failure(term(), term()).

-callback prototype_terminate_child(SN::term(), Context::assembly(), Id::term()) ->
    success() | failure(term(), term()).

%% Assembly::assembly(), Reason::term()
-callback prototype_terminate(SN::term(), Context::assembly()) ->
    success().

%% TODO: We should provide assembly decode/encode format with ability to pass json data through a prototype;
-spec install(Assembly::assembly()) ->
                     success(pid()) | failure(term(), term()).
install(Assembly) ->
    SN = erlmachine_assembly:serial_no(Assembly),
    Prot = erlmachine_assembly:prototype(Assembly),
    Specs = [spec(Assembly, Ext)|| Ext <- erlmachine_assembly:parts(Assembly)],
    Opt = erlmachine_prototype:options(Prot),
    Module = erlmachine_prototype:name(Prot),
    Module:prototype_init(SN, Specs, Opt).

-spec install(Assembly::assembly(), Ext::assembly()) ->
                     success(pid()) | failure(term(), term()).
install(Assembly, Ext, Args) ->
    Module:prototype_start_child(SN, Spec).

-spec uninstall(Assembly::assembly(), Id::term()) ->
                       failure(term(), term()).
uninstall(Assembly, Id) ->
    Module:prototype_terminate_child(SN, Id).

-spec uninstall(Assembly::assembly()) ->
                       success().
uninstall(Assembly) ->
    SN = erlmachine_assembly:serial_no(Assembly),
    Model = erlmachine_assembly:model(Assembly), Prot = erlmachine_model:prototype(Model),
    Module = erlmachine_prototype:name(Prot),
    Module:prototype_terminate(SN).

%% TODO erlmachine_supervisor:install() will be called from prototype;
-spec spec(Assembly::assembly(), Ext::assembly()) -> Spec::map().
spec(Assembly, Ext) ->
    SN = erlmachine_assembly:serial_no(Assembly),
    Env = erlmachine_assembly:env(Assembly),
    Rel = erlmachine_assembly:fixed(erlmachine_assembly:env(Ext, Env), SN),
    Module = erlmachine_assembly:name(Rel),
    %% TODO Start arguments need to be formed outside;
    Start = {Module, install, [Rel]},

    Type = Module:type(), true = (Type == supervisor orelse Type == worker),
    SN = erlmachine_assembly:serial_no(Rel),
    #{
      id => SN,
      start => Start,
      type => Type
     }.

%% TODO https://github.com/rabbitmq/rabbitmq-common/blob/master/src/supervisor2.erl

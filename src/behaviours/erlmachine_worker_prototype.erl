-module(erlmachine_worker_prototype).
%% The main puprouse of the worker prototype is the ability to change transport layer without affecting business layer of application;

%% We may use different schemas:
%% 1. direct function call;
%% 2. gen_server call (via local, global, to use process registry, etc.)
%% 3. through mediation layer like message broker RabbitMQ, Apache Kafka, etc) or server (within Kubernetes cluster or in the cloud like AWS, etc.);

%% Prototype layer can handle the next concerns: overload, error logging, capacity management, etc..

%% erlmachine_assembly
-export([install/1, uninstall/3, attach/4, detach/3]).

%% erlmachine_transmission
-export([transmit/3, load/3, rotate/3, rotation/3]).

-include("erlmachine_factory.hrl").
-include("erlmachine_system.hrl").

-callback prototype_init(SN::serial_no(), Context::term(), Opt::list()) ->
    success(pid()) | failure(term(), term()).

-callback prototype_call(SN::serial_no(), Request::term()) ->
    term().

-callback prototype_cast(SN::serial_no(), Request::term()) ->
    success().

-callback prototype_terminate(SN::serial_no()) ->
    success().

-spec init(Assembly::assembly()) ->
                  success(pid()) | failure(term(), term()).
init(Assembly) ->
    SN = erlmachine_assembly:serial_no(Assembly),
    Model = erlmachine_assembly:model(Assembly), Prot = erlmachine_model:prototype(Model),
    Opt = erlmachine_prototype:options(Prot),
    Module = erlmachine_prototype:name(Prot),
    Module:prototype_init(SN, Assembly, Opt).

-spec terminate(Assembly::assembly()) ->
                       success().
terminate(Assembly) ->
    SN = erlmachine_assembly:serial_no(Assembly),
    Model = erlmachine_assembly:model(Assembly), Prot = erlmachine_model:prototype(Model),
    Module = erlmachine_prototype:name(Prot),
    Module:prototype_terminate(SN).

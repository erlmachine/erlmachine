-module(erlmachine_worker_prototype).
%% NOTE: The main puprouse of the worker prototype is the ability to impact transport layer without affecting business layer of service;
%% NOTE: There are few examples:
%% 1. direct function call;
%% 2. gen_server (via local, global, to use process registry, etc.);
%% 3. gen_server2;
%% 4. gen_batch_server;
%% 5. via message broker RabbitMQ, Apache Kafka, etc);
%% 6. via http server (within Kubernetes cluster or in the cloud like AWS, etc.);

%% NOTE: Worker prototype concerns: overloading, error handling, capacity management, etc..

-export([install/1, rotate/3, transmit/2, uninstall/1]).
-export([init/1, call/2, cast/3, terminate/1]).

-include("erlmachine_factory.hrl").
-include("erlmachine_system.hrl").

-callback prototype_init(SN::serial_no(), Context::term(), Opt::list()) ->
    success(pid()) | failure(term(), term()).

-callback prototype_call(SN::serial_no(), Request::term()) ->
    term().

-callback prototype_cast(SN::serial_no(), Message::term(), Ext::term()) ->
    success().

-callback prototype_terminate(SN::serial_no()) ->
    success().

%%%===================================================================
%%%  Assembly API layer
%%%===================================================================

-spec install(Assembly::assembly()) ->
                  success(pid()) | failure(term(), term()).
install(Assembly) ->
    SN = erlmachine_assembly:serial_no(Assembly),
    Prot = erlmachine_assembly:prototype(Assembly),
    Opt = erlmachine_prototype:options(Prot),
    Name = erlmachine_prototype:name(Prot),
    Name:prototype_init(SN, Assembly, Opt).

-spec uninstall(Assembly::assembly()) ->
                       success().
uninstall(Assembly) ->
    SN = erlmachine_assembly:serial_no(Assembly),
    Prot = erlmachine_assembly:prototype(Assembly),
    Name = erlmachine_prototype:name(Prot),
    Name:prototype_terminate(SN).

%%%===================================================================
%%%  Transmission API layer
%%%===================================================================

-spec rotate(Assembly::assembly(), Motion::term(), Ext::assembly()) -> 
                    success().
rotate(Assembly, Motion, Ext) ->
    SN = erlmachine_assembly:serial_no(Assembly),
    Prot = erlmachine_assembly:prototype(Assembly),
    Id = erlmachine_assembly:label(Ext),
    Name = erlmachine_prototype:name(Prot),
    Name:prototype_cast(SN, Motion, Id).

-spec transmit(Assembly::assembly(), Motion::term()) ->
                      term() | failure(term(), term()).
transmit(Assembly, Motion) ->
    SN = erlmachine_assembly:serial_no(Assembly),
    Prot = erlmachine_assembly:prototype(Assembly),
    Name = erlmachine_prototype:name(Prot),
    Name:prototype_call(SN, Motion).

%%%===================================================================
%%% Prototype API layer
%%%===================================================================

-spec init(Context::assembly()) -> 
                  success(assembly()) | failure(term(), term(), assembly()).
init(Context) ->
    erlmachine:success(Context).

-spec call(Context::assembly(), Request::term()) -> 
                  success(assembly()) | failure(term(), term(), assembly()).
call(Context, _Request) -> 
    erlmachine:success(Context).

-spec cast(Context::assembly(), Message::term(), Ext::assembly()) ->
                  success(assembly()) | failure(term(), term(), assembly()).
cast(Context, Motion, Ext) ->
    %% The rotation is optional and depends on models return;
    erlmachine_transmission:rotate(Ext, Motion),
    erlmachine:success(Context).

-spec terminate(Context::assembly()) -> 
                       success(assembly()) | failure(term(), term(), assembly()).
terminate(Context) ->
    erlmachine:success(Context).

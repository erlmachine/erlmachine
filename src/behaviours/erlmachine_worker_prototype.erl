-module(erlmachine_worker_prototype).
%% NOTE: The main puprouse of the worker prototype is the ability to impact transport layer without affecting business layer of service;
%% NOTE: There are few examples:
%% 1. direct function call;
%% 2. gen_server (via local, global, to use process registry, etc.);
%% 3. gen_server2;
%% 4. gen_batch_server;
%% 5. via message broker RabbitMQ, Apache Kafka, etc);
%% 6. via http server (within Kubernetes cluster or in the cloud like AWS, etc.);

%% NOTE: Worker prototype concerns: overloading, error handling, capacity management, etc.;
%% NOTE: In comparision to erlmachine_supervisor_prototype a worker prototype is statefull;

%% API
-export([start/1]).
-export([rotate/2, transmit/2]).
-export([stop/1]).

%% Context API
-export([init/1, call/2, cast/2, info/2, terminate/2]).

-type context() :: term().

-include("erlmachine_factory.hrl").
-include("erlmachine_assembly.hrl").
-include("erlmachine_system.hrl").

-callback prototype_init(SN::serial_no(), Context::context(), Opt::list()) ->
    success(pid()) | failure(term(), term()).

-callback prototype_call(SN::serial_no(), Req::term()) ->
    term().

-callback prototype_cast(SN::serial_no(), Msg::term()) ->
    success().

-callback prototype_terminate(SN::serial_no()) ->
    success().

%%%===================================================================
%%%  Transmission API
%%%===================================================================

-spec start(Assembly::assembly()) ->
                  success(pid()) | failure(term(), term()).
start(Assembly) ->
    SN = erlmachine_assembly:serial_no(Assembly),

    Prot = erlmachine_assembly:prototype(Assembly),
    Name = erlmachine_prototype:name(Prot), Opt = erlmachine_prototype:options(Prot),

    Name:prototype_init(SN, Assembly, Opt).

-spec rotate(Assembly::assembly(), Msg::term()) ->
                    success().
rotate(Assembly, Msg) ->
    SN = erlmachine_assembly:serial_no(Assembly),

    Prot = erlmachine_assembly:prototype(Assembly),
    Name = erlmachine_prototype:name(Prot),

    Name:prototype_cast(SN, Msg).

-spec transmit(Assembly::assembly(), Req::term()) ->
                      term() | failure(term(), term()).
transmit(Assembly, Req) ->
    SN = erlmachine_assembly:serial_no(Assembly),

    Prot = erlmachine_assembly:prototype(Assembly),
    Name = erlmachine_prototype:name(Prot),

    Name:prototype_call(SN, Req).

-spec stop(Assembly::assembly()) ->
                  success().
stop(Assembly) ->
    SN = erlmachine_assembly:serial_no(Assembly),

    Prot = erlmachine_assembly:prototype(Assembly),
    Name = erlmachine_prototype:name(Prot),

    Name:prototype_terminate(SN).


%%%===================================================================
%%% Prototype API
%%%===================================================================

-spec init(Context::context()) -> 
                  success(context()) | failure(term(), term(), context()).
init(Context) ->
    Assembly = Context,
    erlmachine:success(Assembly).

-spec call(Context::context(), Req::term()) ->
                  success(term(), context()) | failure(term(), term(), context()).
call(Context, _Req) ->
    Assembly = Context,
    erlmachine:success(ok, Assembly).

-spec cast(Context::context(), Msg::term()) ->
                  success(context()) | failure(term(), term(), context()).
cast(Context, _Msg) ->
    Assembly = Context,
    %% The rotation is optional and depends on models return;
    %erlmachine_transmission:rotate(Ext, Motion),
    erlmachine:success(Assembly).

-spec info(Context::context(), Info::term()) ->
                  success(context()) | failure(term(), term(), context()).
info(Context, _Info) ->
    Assembly = Context,
    %% The rotation is optional and depends on models return;
    %erlmachine_transmission:rotate(Ext, Motion),
    erlmachine:success(Assembly).

-spec terminate(Context::assembly(), Reason::term()) -> 
                       success() | failure(term(), term()).
terminate(Context, _Reason) ->
    _Assembly = Context,
    erlmachine:success().

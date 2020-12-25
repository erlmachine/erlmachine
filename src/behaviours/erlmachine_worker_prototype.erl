-module(erlmachine_worker_prototype).
%% NOTE: The main purpouse of the worker prototype is the ability to make impact on transport layer without affecting business layer of service;
%% NOTE: There are few examples:
%% 1. direct function call;
%% 2. gen_server (via local, global, to use process registry, etc.);
%% 3. gen_server2;
%% 4. gen_batch_server;
%% 5. via message broker RabbitMQ, Apache Kafka, etc);
%% 6. via http server (within Kubernetes cluster or in the cloud like AWS, etc.);

%% NOTE: Worker prototype concerns: overloading, error handling, capacity management, etc.;
%% NOTE: In comparision to erlmachine_supervisor_prototype a worker prototype is state-full;
%% NOTE: The responsibility of worker prototype is to terminate running process after error status has returned;
%% NOTE: The responsibility of worker prototype is to catch the all errors outside erlmachine scope;

%% API
-export([boot/1]).

-export([process/2]).
-export([execute/2]).

-export([shutdown/1]).

%% Context API
-export([init/1, call/2, cast/2, info/2, terminate/1]).

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

-spec boot(Assembly::assembly()) ->
                  success(pid()) | failure(term(), term()).
boot(Assembly) ->
    SN = erlmachine_assembly:serial_no(Assembly),

    Prot = erlmachine_assembly:prototype(Assembly),
    Name = erlmachine_prototype:name(Prot), Opt = erlmachine_prototype:options(Prot),

    Name:prototype_init(SN, _Context = Assembly, Opt).

-spec process(Assembly::assembly(), Msg::term()) ->
                    success().
process(Assembly, Msg) ->
    SN = erlmachine_assembly:serial_no(Assembly),

    Prot = erlmachine_assembly:prototype(Assembly),
    Name = erlmachine_prototype:name(Prot),

    Name:prototype_cast(SN, Msg).

-spec execute(Assembly::assembly(), Req::term()) ->
                      term().
execute(Assembly, Req) ->
    SN = erlmachine_assembly:serial_no(Assembly),

    Prot = erlmachine_assembly:prototype(Assembly),
    Name = erlmachine_prototype:name(Prot),

    Name:prototype_call(SN, Req).

-spec shutdown(Assembly::assembly()) ->
                  success().
shutdown(Assembly) ->
    SN = erlmachine_assembly:serial_no(Assembly),

    Prot = erlmachine_assembly:prototype(Assembly),
    Name = erlmachine_prototype:name(Prot),

    Name:prototype_terminate(SN).

%%%===================================================================
%%% Context API
%%%===================================================================

-spec init(Context::context()) ->
                  success(context()) | failure(term(), term(), context()).
init(Context) ->
    erlmachine_worker:boot(Context).

-spec cast(Context::context(), Msg::term()) ->
                  success(context()) | failure(term(), term(), context()).
cast(Context, Msg) ->
    erlmachine_worker:process(Context, Msg).

-spec call(Context::context(), Req::term()) ->
                  success(term(), context()) | failure(term(), term(), context()).
call(Context, Req) ->
    erlmachine_worker:execute(Context, Req).

-spec info(Context::context(), Info::term()) ->
                  success(context()) | failure(term(), term(), context()).
info(Context, Info) ->
    erlmachine_worker:pressure(Context, Info).

-spec terminate(Context::context()) ->
                       success().
terminate(Context) ->
    erlmachine_worker:shutdown(Context).

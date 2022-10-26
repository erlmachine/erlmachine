-module(erlmachine_worker_prototype).
%% TODO: To make prototypes catalogue via auto genereted docs;
%% NOTE: The main purpouse of worker prototype is the ability to change transport layer (in/out API), execution environment (allocated process, dicrect function calls, remote server API, etc.) without affecting business layer of an extension

%% NOTE: There are few examples of transport implementations:

%% 1. direct function call
%% 2. gen_server (via local, global, process registry, etc.)
%% 3. gen_server2
%% 4. gen_batch_server
%% 5. via message broker RabbitMQ, Apache Kafka, etc)
%% 6. via http server (within Kubernetes cluster or in the cloud like AWS, etc.)

%% NOTE: Worker prototype concerns: overloading, error handling, capacity management, etc.
%% NOTE: In comparision to erlmachine_supervisor_prototype a worker prototype is state-full
%% NOTE: The responsibility of worker prototype is to terminate running process after error status has returned
%% NOTE: The responsibility of worker prototype is to catch the all errors outside erlmachine scope

%% API

-export([is_worker_prototype/1]).

-export([startup/1]).

-export([process/2]).
-export([execute/2]).

-export([shutdown/3]).

%% Context API
-export([init/1, call/2, cast/2, info/2, terminate/2]).

-type context() :: term().

-include_lib("erlbox/include/erlbox.hrl").

-include("erlmachine_factory.hrl").
-include("erlmachine_assembly.hrl").

-callback prototype_init(SN::serial_no(), Context::context(), Opt::map()) ->
    success(pid()) | failure(term(), term()).

-callback prototype_call(SN::serial_no(), Req::term()) ->
    term().

-callback prototype_cast(SN::serial_no(), Msg::term()) ->
    success().

-callback prototype_terminate(SN::serial_no(), Reason::term(), Timeout::term()) ->
    success().

-spec is_worker_prototype(Module::atom()) -> boolean().
is_worker_prototype(Module) ->
    lists:member(?MODULE, erlmachine:behaviours(Module)).

%%%  Transmission API

-spec startup(Assembly::assembly()) ->
                  success(pid()) | failure(term(), term()).
startup(Assembly) ->
    SN = erlmachine_assembly:serial_no(Assembly),

    Prot = erlmachine_assembly:prototype(Assembly),
    Module = erlmachine_prototype:module(Prot), Opt = erlmachine_prototype:options(Prot),

    Module:prototype_init(SN, _Context = Assembly, Opt).

-spec process(Assembly::assembly(), Msg::term()) ->
                    success().
process(Assembly, Msg) ->
    SN = erlmachine_assembly:serial_no(Assembly),

    Prot = erlmachine_assembly:prototype(Assembly),
    Module = erlmachine_prototype:module(Prot),

    Module:prototype_cast(SN, Msg).

-spec execute(Assembly::assembly(), Req::term()) ->
                      term().
execute(Assembly, Req) ->
    SN = erlmachine_assembly:serial_no(Assembly),

    Prot = erlmachine_assembly:prototype(Assembly),
    Module = erlmachine_prototype:module(Prot),

    Module:prototype_call(SN, Req).

-spec shutdown(Assembly::assembly(), Reason::term(), Timeout::term()) ->
                  success().
shutdown(Assembly, Reason, Timeout) ->
    SN = erlmachine_assembly:serial_no(Assembly),

    Prot = erlmachine_assembly:prototype(Assembly),
    Module = erlmachine_prototype:module(Prot),

    Module:prototype_terminate(SN, Reason, Timeout).

%%% Context API

-spec init(Context::context()) ->
                  success(context()) | failure(term(), term(), context()).
init(Context) ->
    erlmachine_worker_model:startup(Context).

-spec cast(Context::context(), Msg::term()) ->
                  success(context()) | failure(term(), term(), context()).
cast(Context, Msg) ->
    erlmachine_worker_model:process(Context, Msg).

-spec call(Context::context(), Req::term()) ->
                  success(term(), context()) | failure(term(), term(), context()).
call(Context, Req) ->
    erlmachine_worker_model:execute(Context, Req).

-spec info(Context::context(), Info::term()) ->
                  success(context()) | failure(term(), term(), context()).
info(Context, Info) ->
    erlmachine_worker_model:pressure(Context, Info).

-spec terminate(Context::context(), Reason::term()) ->
                       success().
terminate(Context, Reason) ->
    erlmachine_worker_model:shutdown(Context, Reason).

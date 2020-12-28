-module(erlmachine_supervisor_prototype_default).

-export([name/0]).

-export([init/1]).

-export([prototype_init/4]).
-export([prototype_start_child/3]).
-export([prototype_terminate_child/3]).
-export([prototype_terminate/4]).

-behaviour(supervisor).
-behaviour(erlmachine_supervisor_prototype).

-include("erlmachine_factory.hrl").
-include("erlmachine_system.hrl").

-spec name() -> atom().
name() ->
    ?MODULE.

-spec id(SN::serial_no()) -> 
                         atom().
id(SN) ->
    erlang:binary_to_atom(SN, latin1).

%%%===================================================================
%%%  erlmachine_supervisor_prototype behaviour
%%%===================================================================

-record(init, { specs::[map()], opt::[] }).

-spec prototype_init(SN::serial_no(), Specs::list(map()), Context::term(), Opt::list()) ->
                            success(pid()) | failure(term(), term()).
prototype_init(SN, Specs, Context, Opt) ->
    ok = erlmachine_supervisor_prototype:init(Context, Specs),

    Com = #init{ specs = Specs, opt = Opt },
    supervisor:start_link({local, id(SN)}, ?MODULE, Com).

-spec prototype_start_child(SN::serial_no(), Spec::map(), Context::term()) ->
                                   success(pid()) | failure(term(), term()).
prototype_start_child(SN, Spec, Context) ->
    ok = erlmachine_supervisor_prototype:start_child(Context, Spec),

    supervisor:start_child(id(SN), Spec).

-spec prototype_terminate_child(SN::serial_no(), ID::term(), Context::term()) ->
                                       success().
prototype_terminate_child(SN, ID, Context) ->
    ok = erlmachine_supervisor_prototype:terminate_child(Context, ID),

    supervisor:terminate_child(id(SN), ID).

-spec prototype_terminate(SN::serial_no(), Reason::term(), Timeout::term(), Context::term()) ->
                                 success().
prototype_terminate(SN, Reason, _Timeout, Context) ->
    erlmachine_supervisor_prototype:terminate(Context, Reason),

    Reason = normal,
    exit(whereis(id(SN)), Reason),
    erlmachine:success().

%%%===================================================================
%%%  supervisor behaviour
%%%===================================================================

init(#init{ specs = Specs, opt = Opt }) ->
    Strategy = proplists:get_value(strategy, Opt, one_for_one),
    Int = proplists:get_value(intensity, Opt, 1),
    Per = proplists:get_value(period, Opt, 5),
    erlmachine:success({#{strategy => Strategy, intensity => Int, period => Per}, Specs}).

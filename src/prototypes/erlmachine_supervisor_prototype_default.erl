-module(erlmachine_supervisor_prototype_default).

-export([name/0]).

-export([init/1]).

-export([prototype_init/4]).
-export([prototype_start_child/3]).
-export([prototype_terminate_child/3]).
-export([prototype_terminate/2]).

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

-record(init, { specs::[map()], opts::[] }).

-spec prototype_init(SN::serial_no(), Specs::list(map()), Context::term(), Opts::list()) ->
                            success(pid()) | failure(term(), term()).
prototype_init(SN, Specs, Context, Opts) ->
    Com = #init{ specs=Specs, opts=Opts },
    ok = erlmachine_supervisor_prototype:init(Context),
    supervisor:start_link({local, id(SN)}, ?MODULE, Com).

-spec prototype_start_child(SN::serial_no(), Spec::map(), Context::term()) ->
                                   success(pid()) | failure(term(), term()).
prototype_start_child(SN, Spec, Context) ->
    ok = erlmachine_supervisor_prototype:start_child(Context),
    supervisor:start_child(id(SN), Spec).

-spec prototype_terminate_child(SN::serial_no(), Id::term(), Context::term()) ->
                                       success().
prototype_terminate_child(SN, Id, Context) ->
    ok = erlmachine_supervisor_prototype:terminate_child(Context),
    supervisor:terminate_child(id(SN), Id).

-spec prototype_terminate(SN::serial_no(), Context::term()) ->
                                 success().
prototype_terminate(SN, Context) ->
    ok = erlmachine_supervisor_prototype:terminate(Context),
    Reason = normal,
    exit(whereis(id(SN)), Reason),
    erlmachine:success().

%%%===================================================================
%%%  supervisor behaviour
%%%===================================================================

init(#init{ specs=Specs, opts=Opts }) ->
    Strategy = proplists:get_value(strategy, Opts, one_for_one),
    Int = proplists:get_value(intensity, Opts, 1),
    Per = proplists:get_value(period, Opts, 5),
    erlmachine:success({#{strategy => Strategy, intensity => Int, period => Per}, Specs}).

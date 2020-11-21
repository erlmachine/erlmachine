-module(erlmachine_supervisor_sample_prototype).

-export([init/1]).

-export([prototype_init/4]).
-export([prototype_start_child/3]).
-export([prototype_terminate_child/3]).
-export([prototype_terminate/2]).

-behaviour(supervisor).
-behaviour(erlmachine_supervisor_prototype).

-include("erlmachine_factory.hrl").
-include("erlmachine_system.hrl").

-spec format_name(SN::serial_no()) -> 
                         atom().
format_name(SN) ->
    erlang:binary_to_atom(SN, latin1).

-record(init, { specs::[map()], opts::[] }).

%%%===================================================================
%%%  erlmachine_supervisor_prototype behaviour
%%%===================================================================

-spec prototype_init(SN::serial_no(), Context::term(), Specs::list(map()), Opts::list()) ->
                            success(pid()) | failure(term(), term()).
prototype_init(SN, Context, Specs, Opts) ->
    Com = #init{ specs=Specs, opts=Opts },
    ok = erlmachine_supervisor_prototype:init(Context),
    supervisor:start_link({local, format_name(SN)}, ?MODULE, Com).

-spec prototype_start_child(SN::serial_no(), Context::term(), Spec::map()) ->
                                   success(pid()) | failure(term(), term()).
prototype_start_child(SN, Context, Spec) ->
    ok = erlmachine_supervisor_prototype:start_child(Context),
    supervisor:start_child(format_name(SN), Spec).

-spec prototype_terminate_child(SN::serial_no(), Context::term(), Id::term()) ->
                                       success().
prototype_terminate_child(SN, Context, Id) ->
    ok = erlmachine_supervisor_prototype:terminate_child(Context),
    supervisor:terminate_child(format_name(SN), Id).

-spec prototype_terminate(SN::serial_no(), Context::term()) ->
                                 success().
prototype_terminate(SN, Context) ->
    ok = erlmachine_supervisor_prototype:terminate(Context),
    Reason = normal,
    exit(whereis(format_name(SN)), Reason),
    erlmachine:success().

%%%===================================================================
%%%  supervisor behaviour
%%%===================================================================

init(#init{ specs=Specs, opts=Opts }) ->
    Strategy = proplists:get_value(strategy, Opts, one_for_one),
    Int = proplists:get_value(intensity, Opts, 1),
    Per = proplists:get_value(period, Opts, 5),
    erlmachine:success({#{strategy => Strategy, intensity => Int, period => Per}, Specs}).

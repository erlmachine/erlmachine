-module(erlmachine_supervisor_sample_prototype).

-export([init/1]).

-export([prototype_init/4]).
-export([prototype_start_child/3]).
-export([]).
-export([prototype_terminate/2]).

-behaviour(supervisor).
-behaviour(erlmachine_supervisor_prototype).

-spec format_name(SN::serial_no()) -> 
                         atom().
format_name(SN) ->
    erlang:binary_to_atom(SN, latin1).

-record(install, { specs::[map()], opt::[] }).

%%%===================================================================
%%%  erlmachine_supervisor_prototype behaviour
%%%===================================================================

-spec prototype_init(SN::serial_no(), Context::term(), Exts::list(), Specs::list(map()), Opts::list()) ->
                            success(pid()) | failure(term(), term()).
prototype_init(SN, Context, Exts, Specs, Opts) ->
    Com = #install{ specs=Specs, opt=Opt },
    ok = erlmachine_supervisor_prototype:init(Context, Exts),
    supervisor:start_link({local, format_name(SN)}, ?MODULE, Com).

-spec prototype_start_child(SN::serial_no(), Context::term(), Ext::term(), Spec::map()) ->
                                   success(pid()) | failure(term(), term()).
prototype_start_child(SN, Context, Ext, Spec) ->
    ok = erlmachine_supervisor_prototype:start_child(Context, Ext),
    supervisor:start_child(format_name(SN), Spec).

-spec prototype_terminate_child(SN:serial_no(), Context::term(), Id::term()) ->
                                       success().
prototype_terminate_child(SN, Context, Id) ->
    ok = erlmachine_supervisor_prototype:terminate_child(Context, Id),
    supervisor:terminate_child(format_name(SN), Id).

-spec prototype_terminate(SN::serial_no(), Context::term()) ->
                                 success().
prototype_terminate(SN, Context) ->
    ok = erlmachine_supervisor_prototype:terminate(Context),
    Reason = normal,
    exit(whereis(format_name(Name)), Reason),
    erlmachine:success().

%%%===================================================================
%%%  supervisor behaviour
%%%===================================================================

init(#install{ specs=Specs, opt=Opt }) ->
    Strategy = proplists:get_value(strategy, Opt, one_for_one),
    Int = proplists:get_value(intensity, Opt, 1),
    Per = proplists:get_value(period, Opt, 5),
    erlmachine:success({#{strategy => Strategy, intensity => Int, period => Per}, Specs}).

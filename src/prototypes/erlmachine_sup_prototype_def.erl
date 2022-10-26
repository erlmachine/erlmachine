-module(erlmachine_sup_prototype_def).

-behaviour(supervisor).
-behaviour(erlmachine_supervisor_prototype).

-export([name/0]).

%% erlmachine_supervisor_prototype
-export([prototype_init/4]).
-export([prototype_start_child/3]).
-export([prototype_terminate_child/3]).

%% supervisor
-export([init/1]).

-include_lib("erlbox/include/erlbox.hrl").

-include("erlmachine_factory.hrl").

-spec name() -> atom().
name() ->
    ?MODULE.

-spec id(SN::serial_no()) ->
                         atom().
id(SN) ->
    binary_to_atom(SN).

%%%  erlmachine_supervisor_prototype behaviour

-record(init, { specs::[map()], flags::map() }).

-spec prototype_init(SN::serial_no(), Specs::[map()], Context::term(), Opt::map()) ->
                            success(pid()) | failure(term(), term()).
prototype_init(SN, Specs, Context, Opt) ->
    ok = erlmachine_supervisor_prototype:init(Context),

    Com = #init{ specs = Specs, flags = flags(Opt) },
    supervisor:start_link({local, id(SN)}, ?MODULE, Com).

-spec prototype_start_child(SN::serial_no(), Spec::map(), Context::term()) ->
                                   success(pid()) | failure(term(), term()).
prototype_start_child(SN, Spec, Context) ->
    ok = erlmachine_supervisor_prototype:start_child(Context),

    supervisor:start_child(id(SN), Spec).

-spec prototype_terminate_child(SN::serial_no(), ID::term(), Context::term()) ->
                                       success().
prototype_terminate_child(SN, ID, Context) ->
    ok = erlmachine_supervisor_prototype:terminate_child(Context),

    supervisor:terminate_child(id(SN), ID).

%%%  supervisor behaviour

init(#init{ specs = Specs, flags = Flags }) ->
    Strategy = maps:get(<<"strategy">>, Flags, one_for_one),
    Int = maps:get(<<"intensity">>, Flags, 1),
    Per = maps:get(<<"period">>, Flags, 5),

    erlmachine:success({#{ strategy => Strategy, intensity => Int, period => Per }, Specs}).

-spec flags(Opt::map()) -> map().
flags(Opt) ->
    Flags = maps:get(<<"flags">>, Opt, #{}),

    maps:fold(fun fold/3, #{}, Flags).

fold(Key = <<"strategy">>, Value, Acc) when Value == <<"one_for_one">>;
                                            Value == <<"one_for_all">>;
                                            Value == <<"rest_for_one">>;
                                            Value == <<"simple_one_for_one">> ->
    Acc#{ Key => binary_to_atom(Value) };

fold(Key = <<"intensity">>, Value, Acc) when is_integer(Value) ->
    Acc#{ Key => Value };

fold(Key = <<"period">>, Value, Acc) when is_integer(Value) ->
    Acc#{ Key => Value };

fold(Key, Value, Acc) ->
    Acc#{ Key => Value }.


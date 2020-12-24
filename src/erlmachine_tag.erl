-module(erlmachine_tag).

-export([pid/2]).
-export([error/3]).

-include("erlmachine_assembly.hrl").
-include("erlmachine_system.hrl").

-spec pid(Assembly::assembly(), Pid::pid()) -> assembly().
pid(Assembly, Pid) ->
    erlmachine:tag(Assembly, {pid, Pid}).

-spec error(Assembly::assembly(), E::term(), R::term()) -> assembly().
error(Assembly, E, R) ->
    erlmachine:tag(Assembly, {error, {E, R}}).

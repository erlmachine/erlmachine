-module(erlmachine_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init(Options) ->
    Strategy = one_for_all,
    ErlmachineFactorySpec = #{id => erlmachine_factory, start => {erlmachine_factory, start_link, []}},
    ErlmachineTrackerSpec = #{id => erlmachine_tracker, start => {erlmachine_tracker, start_link, []}},
    Specs = [ErlmachineFactorySpec, ErlmachineTrackerSpec],
    {ok, {#{strategy => Strategy}, Specs}}.

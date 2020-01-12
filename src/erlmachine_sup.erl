-module(erlmachine_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    FactoryDriver = erlmachine_factory,
    TrackerDriver = erlmachine_tracker,

    Fun = start_link, Args = [],

    Strategy = one_for_all,

    Specs = [
             #{id => FactoryDriver, start => {FactoryDriver, Fun, Args}},
             #{id => TrackerDriver, start => {TrackerDriver, Fun, Args}}
            ],

    {ok, {#{strategy => Strategy}, Specs}}.

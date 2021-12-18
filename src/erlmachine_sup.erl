-module(erlmachine_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    System = erlmachine_system,

    Transmission = erlmachine_transmission,
    Factory = erlmachine_factory,

    Fun = start_link,
    Args = [],

    Procs = [
             #{ 'id' => System, 'start' => {System, Fun, Args} },

             #{ 'id' => Transmission, 'start' => {Transmission, Fun, Args} },
             #{ 'id' => Factory, 'start' => {Factory, Fun, Args} }
            ],
    {ok, {#{ 'strategy' => one_for_all }, Procs}}.
 

-module(erlmachine_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    Factory = erlmachine_factory,

    Fun = start_link,
    Args = [],

    Procs = [
             #{ 'id' => Factory, 'start' => {Factory, Fun, Args} }
            ],
    {ok, {#{ 'strategy' => one_for_all }, Procs}}.

-module(axle_base_prototype).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).
%% I guess we need to provide the concept of broken detail monitoring, supervisor component can provide that;
%% Procs will be provided by datasheet;
%% Restart strategy will be provided as count of broken elements per sec;
init([]) ->
	Procs = [],
	{ok, {{one_for_one, 1, 5}, Procs}}.

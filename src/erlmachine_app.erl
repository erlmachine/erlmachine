-module(erlmachine_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
	erlmachine_sup:start_link().

stop(_State) ->
	ok.

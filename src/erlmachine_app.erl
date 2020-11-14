-module(erlmachine_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

-export([start_phase/3]).

start(_Type, _Args) ->
    Nodes = [node()],
    mnesia:create_schema(Nodes), ok = mnesia:start(),

    erlmachine_sup:start_link().


stop(_State) ->
    ok = mnesia:stop().

start_phase(_, _Type, _PhaseArgs) ->
    ok.


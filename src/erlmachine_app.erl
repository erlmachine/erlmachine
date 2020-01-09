-module(erlmachine_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

-export([start_phase/3]).

start(_Type, _Args) ->
    Nodes = [node()],
    Schema = mnesia:create_schema(Nodes), ok = mnesia:start(),
 
    io:format("~nSchema res: ~p~n", [Schema]),
    erlmachine_sup:start_link().


stop(_State) ->	erlmachine:success().

start_phase(erlmachine_filesystem, _Type, _PhaseArgs) ->
    ok;
start_phase(_, _Type, _PhaseArgs) ->
    ok.

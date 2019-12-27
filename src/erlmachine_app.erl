-module(erlmachine_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

-export([start_phase/3]).

start(_Type, _Args) ->
    mnesia:create_schema([node()]), ok = mnesia:start(),
    erlmachine_sup:start_link().


stop(_State) ->	erlmachine:success().

start_phase(erlmachine_filesystem, _Type, _PhaseArgs) ->
    ok;
start_phase(_, _Type, _PhaseArgs) ->
    ok.

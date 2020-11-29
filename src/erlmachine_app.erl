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

start_phase(wait_for_tables, _Type, Timeout) when is_integer(Timeout) ->
    Factory = erlmachine_factory:tabname(),
    ok = mnesia:wait_for_tables([Factory], Timeout);

start_phase(add_schema, _Type, File) ->
    Priv = erlmachine:priv_dir(), Path = filename:join(Priv, File),
    [Schema] = jsx:consult(Path, [return_maps]),
    Key = erlmachine_datasheet:schema(),
    ok = jesse:add_schema(Key, Schema);

start_phase(_, _Type, _PhaseArgs) ->
    ok.

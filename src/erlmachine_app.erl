-module(erlmachine_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

-export([start_phase/3]).

-export([wait_for_tables/1, add_schema/1]).

-include("erlmachine_system.hrl").

start(_Type, _Args) ->
    Nodes = [node()],
    mnesia:create_schema(Nodes), ok = mnesia:start(),

    erlmachine_sup:start_link().

stop(_State) ->
    ok = mnesia:stop().

start_phase(wait_for_tables, _Type, Timeout) when is_integer(Timeout) ->
    ok = wait_for_tables(Timeout);

start_phase(add_schema, _Type, Files) when is_list(Files) ->
    [ok = add_schema(File) || File <- Files], 
    erlmachine:success();

start_phase(_, _Type, _PhaseArgs) ->
    erlmachine:success().

-spec wait_for_tables(Timeout::integer()) -> ok.
wait_for_tables(Timeout) ->
    Factory = erlmachine_factory:tabname(),
    ok = mnesia:wait_for_tables([Factory], Timeout).

-spec add_schema(File::list()) -> success().
add_schema(File) ->
    Priv = erlmachine:priv_dir(),
    Path = filename:join(Priv, File), [Schema] = jsx:consult(Path, [return_maps]),
    ok = jesse:add_schema(File, Schema).

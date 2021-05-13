-module(erlmachine_app).

%% “Πάντα ῥεῖ (Panta rhei)  – Everything flows”  ( Heraclitus of Ephesus, ca. 500 BCE)”
%% Excerpt From: Morrison, J. Paul. “Flow-Based Programming - 2nd Edition.” Apple Books.

-behaviour(application).

-export([start/2]).
-export([stop/1]).

-export([start_phase/3]).

-export([wait_for_tables/2, add_schema/1]).

-include("erlmachine_system.hrl").

start(_Type, _Args) ->
    Nodes = [node()],
    erlmachine_database:create_schema(Nodes), ok = erlmachine_database:start(),

    [erlmachine_database:create_table(Tab) || Tab <- tables()],
    erlmachine_sup:start_link().

stop(_State) ->
    ok = erlmachine_database:stop().

start_phase(wait_for_tables, _Type, Timeout) when is_integer(Timeout) ->
    Tables = tables(),

    ok = wait_for_tables(Tables, Timeout);

start_phase(add_schema, _Type, Files) when is_list(Files) ->
    [ok = add_schema(File) || File <- Files],

    erlmachine:success();

start_phase(_, _Type, _PhaseArgs) ->
    erlmachine:success().

-spec wait_for_tables(Tables::[atom()], Timeout::integer()) -> success().
wait_for_tables(Tables, Timeout) ->
    ok = erlmachine_database:wait_for_tables(Tables, Timeout).

-spec add_schema(File::list()) -> success().
add_schema(File) ->
    Priv = erlmachine:priv_dir(), Path = filename:join(Priv, File),
    [Schema] = jsx:consult(Path, [return_maps]),
    ok = jesse:add_schema(File, Schema).

-spec tables() -> [atom()].
tables() ->
    {ok, Modules} = erlmachine:get_key(modules),
    [Module || Module <- Modules, erlmachine_database:is_database(Module)].

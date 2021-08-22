-module(erlmachine_app).

%% “Πάντα ῥεῖ (Panta rhei)  – Everything flows”  ( Heraclitus of Ephesus, ca. 500 BCE)”
%% Excerpt From: Morrison, J. Paul. “Flow-Based Programming - 2nd Edition.” Apple Books.

-behaviour(application).

-export([start/2]).
-export([stop/1]).

-export([start_phase/3]).

-include("erlmachine_system.hrl").

start(_Type, _Args) ->
    Nodes = [node()],
    Modules = erlmachine:modules(),

    erlmachine_db:create_schema(Nodes), ok = erlmachine_db:start(),

    [erlmachine_db:create_table(T) || T <- tables(Modules)],
    erlmachine_sup:start_link().

stop(_State) ->
    ok = erlmachine_db:stop().

start_phase(wait_for_tables, _Type, Timeout) when is_integer(Timeout) ->
    Modules = erlmachine:modules(),

    Tables = [T || T <- tables(Modules)],
    ok = erlmachine_db:wait_for_tables(Tables, Timeout);

start_phase(add_schema, _Type, _)  ->
    Modules = erlmachine:modules(),

    Templates = [T || T <- templates(Modules)],
    [ok = erlmachine_template:add_schema(T) || T <- Templates],

    erlmachine:success();

start_phase(_, _Type, _PhaseArgs) ->
    erlmachine:success().

-spec tables(Modules::[atom()]) -> [atom()].
tables(Modules) ->
    [M || M <- Modules, erlmachine_db:is_db(M)].

-spec templates(Modules::[atom()]) -> [atom()].
templates(Modules) ->
    [M || M <- Modules, erlmachine_template:is_template(M)].

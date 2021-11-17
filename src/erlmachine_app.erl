-module(erlmachine_app).

%% “Πάντα ῥεῖ (Panta rhei)  – Everything flows”  ( Heraclitus of Ephesus, ca. 500 BCE)”
%% Excerpt From: Morrison, J. Paul. “Flow-Based Programming - 2nd Edition.” Apple Books.

-behaviour(application).

-export([start/2]).
-export([stop/1]).

-export([start_phase/3]).

-include("erlmachine_system.hrl").

start(_Type, _Args) ->
    Nodes = [node()], mnesia:create_schema(Nodes),

    ok = mnesia:start(),

    Tables = erlmachine:tables(),
    [erlmachine_table:create(T) || T <- Tables],

    Templates = erlmachine:templates(),
    [ok = erlmachine_template:add_schema(T) || T <- Templates],

    Scopes = erlmachine:scopes(),
    ok = syn:add_node_to_scopes(Scopes),

    erlmachine_sup:start_link().

stop(_State) ->
    ok = mnesia:stop().

start_phase('wait_for_tables', _Type, Timeout) when is_integer(Timeout) ->
    Tables = erlmachine:tables(),

    ok = mnesia:wait_for_tables(Tables, Timeout);

start_phase(_, _Type, _PhaseArgs) ->
    erlmachine:success().



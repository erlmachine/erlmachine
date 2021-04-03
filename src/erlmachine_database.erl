-module(erlmachine_database).

-export([is_database/1]).

-export([create_schema/1]).

-export([create_table/1]).
-export([update_counter/1, update_counter/2, update_counter/3]).
-export([delete_table/1]).

-export([wait_for_tables/2]).

-export([delete_schema/1]).

-export([start/0, stop/0]).
-export([info/0]).

-include("erlmachine_system.hrl").

-type table() :: atom().
-type field() :: atom().

-type key() :: atom().
-type value() :: term().

-export_types([table/0, field/0]).

-callback table() -> table().

-callback attributes() -> [field()].

-callback index() -> [field()].

-callback access_mode() -> 'read_write' | 'read_only'.

-optional_callbacks([index/0, access_mode/0]).

%%% Modules

-spec is_database(Module::atom()) -> boolean().
is_database(Module) ->
    lists:member(?MODULE, erlmachine:behaviours(Module)).

-spec create_schema(Nodes::[node()]) -> success() | failure(term()).
create_schema(Nodes) ->
    mnesia:create_schema(Nodes).

%%% Tables

-spec create_table(Module::atom()) -> success().
create_table(Module) ->
    Table = Module:table(),

    Attributes = Module:attributes(),
    Index = erlmachine:optional_callback(Module, 'index', [], []),
    AccessMode = erlmachine:optional_callback(Module, 'access_mode', [], 'read_write'),

    mnesia:create_table(Table, [{attributes, Attributes}, {index, Index}, {access_mode, AccessMode}]),
    ok.

-spec update_counter(Tab::table()) -> integer().
update_counter(Tab) ->
    update_counter(Tab, _Key = Tab).

-spec update_counter(Tab::table(), Key::key()) -> integer().
update_counter(Tab, Key) ->
    update_counter(Tab, Key, _Default = 1).

-spec update_counter(Tab::table(), Key::key(), Val::value()) -> integer().
update_counter(Tab, Key, Val) when is_integer(Val) ->
    mnesia:dirty_update_counter(Tab, Key, Val).

-spec delete_table(Tab::table()) -> success().
delete_table(Tab) ->
    mnesia:delete_table(Tab),
    ok.

-spec wait_for_tables(Tables::[table()], Timeout::integer()) -> success().
wait_for_tables(Tables, Timeout) ->
    ok = mnesia:wait_for_tables(Tables, Timeout).

-spec delete_schema(Nodes::[node()]) -> success() | failure(term()).
delete_schema(Nodes) ->
    mnesia:create_schema(Nodes).

%%% Database

-spec start() -> success().
start() ->
    ok = mnesia:start().

-spec stop() -> success().
stop() ->
    ok = mnesia:stop().

-spec info() -> success().
info() ->
    mnesia:info().

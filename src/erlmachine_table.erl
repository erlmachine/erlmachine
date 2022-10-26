-module(erlmachine_table).

-export([is_table/1, tables/1]).

-export([create/1]).
-export([inc/1, inc/2]).
-export([delete/1]).

-include_lib("erlbox/include/erlbox.hrl").

-type table() :: atom().
-type field() :: atom().

-type key() :: atom().

-export_types([table/0, field/0]).

-callback table() -> table().

-callback attributes() -> [field()].

-callback index() -> [field()].

-callback access_mode() -> 'read_write' | 'read_only'.

-optional_callbacks([index/0, access_mode/0]).

%%% Modules API

-spec is_table(Module::module()) -> boolean().
is_table(Module) ->
    lists:member(?MODULE, erlmachine:behaviours(Module)).

-spec tables(Modules::[module()]) -> [module()].
tables(Modules) ->
    [M || M <- Modules, is_table(M)].

%%% Tables API

-spec create(Module::module()) -> success().
create(Module) ->
    Tab = Module:table(),

    Attrs = Module:attributes(),
    Index = erlmachine:optional_callback(Module, 'index', [], []),
    Mode = erlmachine:optional_callback(Module, 'access_mode', [], 'read_write'),

    mnesia:create_table(Tab, [{attributes, Attrs}, {index, Index}, {access_mode, Mode}]),
    ok.

-spec inc(Module::module()) -> integer().
inc(Module) ->
    Tab = Module:table(),

    inc(Tab, _Key = Tab).

-spec inc(Module::module(), Key::key()) -> integer().
inc(Module, Key) ->
    Tab = Module:table(),

    mnesia:dirty_update_counter(Tab, Key, _Val = 1).

-spec delete(Module::module()) -> success().
delete(Module) ->
    Tab = Module:table(),

    mnesia:delete_table(Tab),
    ok.

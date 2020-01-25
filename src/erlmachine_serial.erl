-module(erlmachine_serial).

-export([create/1, read/1, update/1, update/2, delete/1]).

-export([tabname/0, record_name/0, attributes/0]).

-include("erlmachine_system.hrl").

-type serial()::integer().

-export_type([serial/0]).

-record (serial, { id::atom(), count::integer() }).

-spec tabname() -> atom().
tabname() ->
    ?MODULE.

-spec record_name() -> atom().
record_name() ->
    serial.

-spec attributes() -> list(atom()).
attributes() ->
    record_info(fields, serial).

create(_) ->
    throw(?LINE).

read(_) ->
    throw(?LINE).

-spec update(ID::atom()) -> success(integer()).
update(ID) ->
    erlmachine:success(update(ID, 1)).

-spec update(ID::atom(), Value::integer()) -> success(integer()).
update(ID, Value) ->
    mnesia:dirty_update_counter(tabname(), ID, Value).

delete(_) ->
    throw(?LINE).

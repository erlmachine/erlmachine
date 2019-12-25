-module(erlmachine_serial).

-export([default/0]).
-export([inc/1]).

-export([tracking_no/0, tracking_no/1]).
-export([serial_no/0, serial_no/1]).

-include("erlmachine_system.hrl").

-type serial()::integer().

-export_type([serial/0]).

%% At this point we provide persisnence layer over serial counter;
%% Until persistence layer exists we can be sureabout uniqueness of SN;
%% When persistence layer is lost it's usually about both kind of data (seed and actually data itself);

-spec default() -> serial().
default() ->
    0.

-spec inc(Serial::serial()) -> serial().
inc(Serial) ->
    Serial + 1.

-record (serial, {serial :: integer(), ts :: integer()}).

-spec tracking_no() -> 
                         success(Serial::integer()) | failure(E::term(), R::term()).
tracking_no() -> 
    read(tracking_no).

-spec serial_no() -> 
                 success(Serial::integer()) | failure(E::term(), R::term()).
serial_no() -> 
    read(serial_no).

-spec tracking_no(Serial::serial()) -> 
                         success(Serial::integer()) | failure(E::term(), R::term()).
tracking_no(Serial) -> 
    write(tracking_no, Serial).

-spec serial_no(Serial::serial()) -> 
                       success(Serial::integer()) | failure(E::term(), R::term()).
serial_no(Serial) -> 
    write(serial_no, Serial).



-spec read(Tab::atom()) -> success(Serial::integer()) | failure(E::term(), R::term()).
read(_Tab) ->
    Seed =
        case erlmachine_filesystem:read(<<"">>) of
           {ok, [Num]} ->
                {ok, Num};
            {ok, _} ->
                {ok, default()};
            {error, _} = Error ->
                Error
        end,
    Seed.

%% At that place we consider to rewrite file instead of append;
-spec write(Tab::atom(), Serial::serial()) -> success() | failure(E::term(), R::term()).
write(_Tab, Serial) ->
    _Record = #serial{},
    Status = erlmachine_filesystem:write(<<"">>, [Serial]),
    Status.

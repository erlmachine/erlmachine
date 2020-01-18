-module(erlmachine_serial_no).

-export([serial_no/1, serial_no/2]).

-type serial_no()::binary().

-export_type([serial_no/0]).

-type serial()::erlmachine_serial:serial().

-spec serial_no(Serial::serial()) -> serial_no().
serial_no(Serial) ->
    <<_:32, _:32, _:32, _:32>> = erlmachine:guid(Serial).

-spec serial_no(Serial::serial(), SN::serial_no()) -> serial_no().
serial_no(Serial, <<B1:32, B2:32, B3:32, B4:32>>) ->
    B5 = erlang:phash2({B1, Serial}, 4294967296),
    <<(B2 bxor B5):32, (B3 bxor B5):32, (B4 bxor B5):32, B5:32>>.

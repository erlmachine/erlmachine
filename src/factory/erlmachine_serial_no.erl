-module(erlmachine_serial_no).

-export([serial_no/1, serial_no/2]).

-type serial_no()::binary().

-export_type([serial_no/0]).

-type seed()::erlmachine:seed().

-spec serial_no(Seed::seed()) -> serial_no().
serial_no(Seed) ->
    <<_:32, _:32, _:32, _:32>> = erlmachine:guid(Seed).

-spec serial_no(Seed::seed(), SN::serial_no()) -> serial_no().
serial_no(Seed, <<B1:32, B2:32, B3:32, B4:32>>) ->
    B5 = erlang:phash2({B1, Seed}, 4294967296),
    <<(B2 bxor B5):32, (B3 bxor B5):32, (B4 bxor B5):32, B5:32>>.

-module(erlmachine_serial_no).

-export([serial_no/1, no/1, no/2]).

-type serial_no()::binary().

-record(no, {b1::binary(), b2::binary(), b3::binary(), b4::binary()}).

-type no()::#no{}.

-export_type([serial_no/0, no/0]).

-type serial()::erlmachine:serial().

%% API.

%% generate a readable string representation of a SN.
%%
%% base64url encoding was provided; 
%% This format is safer and more applicable by web (in comparison with base64);

-spec base64url(Hash::binary()) -> Base64::binary().
base64url(Hash) when is_binary(Hash) ->
    Base64 = base64:encode(Hash),
    Base64.

-spec no(Serial::serial()) -> no().
no(Serial) ->
    GUID = erlmachine:guid(Serial),
    <<B1:32, B2:32, B3:32, B4:32>> = GUID,
    #no{b1=B1, b2=B2, b3=B3, b4=B4}.

-spec no(No::no(), Serial::serial()) -> no().
no(#no{b1=B1, b2=B2, b3=B3, b4=B4}, Serial) ->
    B5 = erlang:phash2({B1, Serial}, 4294967296),
    #no{b1=(B2 bxor B5), b2=(B3 bxor B5), b3=(B4 bxor B5), b4=B5}.

-spec serial_no(no()) -> serial_no().
serial_no(#no{b1=B1, b2=B2, b3=B3, b4=B4}) ->
    Hash = <<B1:32, B2:32, B3:32, B4:32>>,
    SN = base64url(Hash),
    SN.

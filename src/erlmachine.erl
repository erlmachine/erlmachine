-module(erlmachine).

-folder(<<"erlmachine">>).

-export([serial_no/1]).

-export([start/0, stop/0]).

-export([motion/1, motion/2]).
-export([envelope/1, header/1, body/1]).

-export([command/1, command/2]).
-export([document/1, document/2]).
-export([event/1, event/2]).
-export([request_reply/2, request_reply/3]).

-export([failure/1, failure/2, failure/3]).
-export([success/0, success/1, success/2]).

-export([attribute/3]).

-export([guid/1]).

-export([digest/1, digest/2]).
-export([base64url/1]).

-export([timestamp/0]).

-include("erlmachine_system.hrl").
-include("erlmachine_factory.hrl").
-include("erlmachine_filesystem.hrl").

-type motion() :: erlmachine_transmission:motion().

-type envelope() :: erlmachine_transmission:envelope().

-type header() :: erlmachine_transmission:header().

-type body() :: erlmachine_transmission:body().

-type serial() :: erlmachine_serial:serial().

-record(guid, {node::node(), reference::reference(), serial::serial()}).

%% The main purpouse of erlmachine project is providing a set of well designed behaviours which are accompanied with visualization tools as well.
%%  Erlmachine doesn't restrict your workflow by the one possible way but instead provide to you ability to implement your own components. This ability is available under flexible mechanism of prototypes and overloading.  

-spec serial_no(Assembly::assembly()) -> serial_no().
serial_no(Assembly) ->
    erlmachine_assembly:serial_no(Assembly).

-spec start() -> success().
start() ->
    {ok, _} = application:ensure_all_started(erlmachine),
    success().

-spec stop() -> success() | failure(Reason :: any()).
stop() ->
    application:stop(erlmachine).

-spec motion(Body::term()) -> motion().
motion(Body) ->
    erlmachine_transmission:motion(Body).

-spec motion(Header::header(), Body::body()) -> motion().
motion(Header, Body) ->
    erlmachine_transmission:motion(Header, Body).

-spec envelope(Motion::motion()) -> envelope().
envelope(Motion) ->
    erlmachine_transmission:envelope(Motion).

-spec header(Motion::motion()) -> header().
header(Motion) ->
    erlmachine_transmission:header(Motion).

-spec body(Motion::motion()) -> body().
body(Motion) ->
    erlmachine_transmission:body(Motion).

-spec command(Body::body()) -> motion(). 
command(Body) ->
    erlmachine_transmission:command(Body).

-spec command(Header::header(), Body::body()) -> motion().
command(Header, Body) ->
    erlmachine_transmission:command(Header, Body).

-spec document(Body::body()) -> motion(). 
document(Body) ->
    erlmachine_transmission:document(Body).

-spec document(Header::header(), Body::body()) -> motion().
document(Header, Body) ->
    erlmachine_transmission:document(Header, Body).

-spec event(Body::body()) -> motion(). 
event(Body) ->
    erlmachine_transmission:event(Body).

-spec event(Header::header(), Body::body()) -> motion().
event(Header, Body) ->
    erlmachine_transmission:event(Header, Body).

-spec request_reply(Body::body(), Address::term()) -> motion().
request_reply(Body, Address) ->
    erlmachine_transmission:request_reply(Body, Address).

-spec request_reply(Header::header(), Body::body(), Address::term()) -> motion().
request_reply(Header, Body, Address) ->
    erlmachine_transmission:request_reply(Header, Body, Address).


-spec failure(E::term(), R::term()) -> failure(E::term(), R::term()).
failure(E, R) -> 
    erlmachine_system:failure(E, R).

-spec failure(E::term()) -> failure(E::term()).
failure(E) ->
    erlmachine_system:failure(E).

-spec failure(E::term(), R::term(), S::term()) -> failure(E::term(), R::term(), S::term()).
failure(E, R, S) -> 
    erlmachine_system:failure(E, R, S).

-spec success(Result::term()) -> success(Result::term()).
success(Result) ->
    erlmachine_system:success(Result).

-spec success(Result::term(), State::term()) -> success(Result::term(), State::term()).
success(Result, State) -> 
    erlmachine_system:success(Result, State).

-spec success() -> success().
success() ->
    erlmachine_system:success().

-spec attribute(Module::atom(), Tag::atom(), Default::term()) -> false | {Tag::atom(), Value::term()}.
attribute(Module, Tag, Default) ->
    Attributes = Module:module_info(attributes),
    Result = lists:keyfind(Tag, 1, Attributes),
    case 
        Result of false -> Default;
        {Tag, Data} -> Data 
    end.

%% generate a readable string representation of a SN/MN/PN/TN.
%%
%% base64url encoding was provided; 
%% This format is safer and more applicable by web (in comparison with base64);

-spec guid(Serial::serial()) -> binary().
guid(Serial) ->
    GUID = #guid{node=node(), serial=Serial, reference=make_ref()},
    digest(GUID).

-spec digest(Data::term(), base64 | base64url) -> binary().
digest(Data, base64) ->
    base64:encode(digest(Data));

digest(Data, base64url) ->
    base64url(digest(Data)).

-spec digest(Data::term()) -> binary().
digest(Data) ->
    MD5 = erlang:md5(term_to_binary(Data)),
    MD5.

-spec base64url(N::binary()) -> Base64::binary().
base64url(N) when is_binary(N) ->
    Base64 = base64:encode(N),
    Base64Url = [fun($+) -> <<"-">>; ($/) -> <<"_">>; (C) -> <<C>> end(Char)|| <<Char>> <= Base64],
    << <<X/binary>> || X <- Base64Url >>.

-spec timestamp() -> integer().
timestamp() ->
    erlang:system_time(seconds).

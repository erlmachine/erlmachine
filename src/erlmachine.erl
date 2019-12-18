-module(erlmachine).

-folder(<<"erlmachine">>).

-export([start/0, stop/0]).

-export([motion/1, motion/2]).
-export([envelope/1, header/1, body/1, body/2, property/2, property/3]).

-export([command/1, command/2]).
-export([document/1, document/2]).
-export([event/1, event/2]).
-export([request_reply/2, request_reply/3]).

-export([failure/1, failure/2, failure/3]).
-export([success/0, success/1, success/2]).

-export([guid/0, guid/1]).
-export([serial/0, serial/1]).
-export([read_serial/1, write_serial/2]).

-export([attribute/3]).

-include("erlmachine_system.hrl").
-include("erlmachine_filesystem.hrl").

-type serial()::integer().

-type motion() :: erlmachine_transmission:motion().

-type envelope() :: erlmachine_transmission:envelope().

-type header() :: erlmachine_transmission:header().

-type body() :: erlmachine_transmission:body().

-record(guid, {node::node(), reference::reference(), serial::serial()}).

-type guid()::#guid{}.

-export_types([serial/0, guid/0]).

%% The main purpouse of erlmachine project is providing a set of well designed behaviours which are accompanied with visualization tools as well.
%%  Erlmachine doesn't restrict your workflow by the one possible way but instead provide to you ability to implement your own components. This ability is available under flexible mechanism of prototypes and overloading.  

-spec start() -> success().
start() ->
    {ok, _} = application:ensure_all_started(erlmachine),
    success().

-spec stop() -> success() | failure(Reason :: any()).
stop() ->
    application:stop(erlmachine).

-spec attribute(Module::atom(), Tag::atom(), Default::term()) -> false | {Tag::atom(), Value::term()}.
attribute(Module, Tag, Default) ->
    Attributes = Module:module_info(attributes),
    Result = lists:keyfind(Tag, 1, Attributes),
    case 
        Result of false -> Default;
        {Tag, Data} -> Data 
    end.

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

-spec property(Id::term(), Motion::motion()) -> term().
property(Id, Motion) ->
    property(Id, Motion, undefined).

-spec property(Id::term(), Motion::motion(), Default::term()) -> term().
property(Id, Motion, Default) ->
    Header = erlmachine_transmission:header(Motion),
    maps:get(Id, Header, Default).

-spec body(Motion::motion()) -> body().
body(Motion) ->
    erlmachine_transmission:body(Motion).

-spec body(Motion::motion(), Body::body()) -> motion().
body(Motion, Body) ->
    maps:put(body, Body, Motion).

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

-spec failure(E::term(), R::term(), State::term()) -> failure(E::term(), R::term(), State::term()).
failure(E, R, State) -> 
    erlmachine_system:failure(E, R, State).

-spec success(Result::term()) -> success(Result::term()).
success(Result) ->
    erlmachine_system:success(Result).

-spec success(Result::term(), State::term()) -> success(Result::term(), State::term()).
success(Result, State) -> 
    erlmachine_system:success(Result, State).

-spec success() -> success().
success() ->
    erlmachine_system:success().

-spec guid() -> GUID::guid().
guid() ->
    guid(0).

-spec guid(Serial::serial()) -> GUID::guid().
guid(Serial) ->
    GUID = #guid{node=node(), serial=Serial, reference=make_ref()},
    MD5 = erlang:md5(term_to_binary(GUID)),
    MD5.

-spec serial() -> serial().
serial() ->
    0.

-spec serial(Serial::serial()) -> serial().
serial(Serial) ->
    Serial + 1.

%% At this point we provide persisnence layer over serial counter;
%% Until persistence layer exists we can be sureabout uniqueness of SN;
%% When persistence layer is lost it's usually about both kind of data (seed and actually data itself);

-spec read_serial(Path::path()) -> success(Serial::integer()) | failure(E::term(), R::term()).
read_serial(Path) ->
    Serial =
        case erlmachine_filesystem:read(Path) of
           {ok, [Num]} ->
                {ok, Num};
            {ok, _} ->
                {ok, serial()};
            {error, _} = Error ->
                Error
        end,
    Serial.

%% At that place we consider to rewrite file instead of append;
-spec write_serial(Path::path(), Serial::integer()) -> success() | failure(E::term(), R::term()).
write_serial(Path, Serial) ->
    Result = erlmachine_filesystem:write(Path, [Serial]),
    Result.

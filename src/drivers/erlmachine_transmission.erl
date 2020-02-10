-module(erlmachine_transmission).
-behaviour(gen_server).

%% API.
-export([start_link/0]).
 
%% gen_server.
-export([
         init/1, 
         handle_call/3, handle_cast/2, handle_info/2, 
         terminate/2,
         code_change/3
        ]).

-export([rotate/3, transmit/3]).
-export([rotation/3]).
-export([transmission/3]).

-export([motion/1, motion/2, envelope/1, header/1, body/1]).
-export([command/1, command/2]).
-export([document/1, document/2]).
-export([event/1, event/2]).
-export([request_reply/2, request_reply/3, request_reply/4]).

%% Transmission will be loaded directly by call where ID argument is provided; 
%% Transmission can be represented by a lot of copies where each of them is marked by unique serial number;

-include("erlmachine_factory.hrl").
-include("erlmachine_system.hrl").

-type motion() :: map().

-type envelope() :: map().

-type header() :: map().

-type body() :: term().

-export_type([motion/0, envelope/0, header/0, body/0]).

-spec rotate(GearBox::assembly(), Label::term(), Motion::term()) ->
                    term().
rotate(GearBox, Label, Motion) ->
    Part = erlmachine_gearbox:find(GearBox, Label),
    rotation(GearBox, Part, Motion).

-spec transmit(GearBox::assembly(), Label::term(), Motion::term()) ->
                      term().
transmit(GearBox, Label, Motion) ->
    Part = erlmachine_gearbox:find(GearBox, Label),
    transmission(GearBox, Part, Motion).

-spec rotation(GearBox::assembly(), Part::assembly(), Motion::term()) -> 
                         term().
rotation(GearBox, Part, Motion) -> 
    SN = erlmachine_assembly:serial_no(Part),
    (erlmachine_assembly:prototype_name(Part)):rotate(SN, GearBox, Part, Motion).

-spec transmission(GearBox::assembly(), Part::assembly(), Motion::term()) ->
                      term().
transmission(GearBox, Part, Motion) ->
    SN = erlmachine_assembly:serial_no(Part),
    (erlmachine_assembly:prototype_name(Part)):transmit(SN, GearBox, Part, Motion).


-record(state, {
}).

%% A message consists of two basic parts:
%% 1. Header – Information used by the messaging system that describes the data being transmitted, its origin, its destination, and so on.
%% 2. Body – The data being transmitted; generally ignored by the messaging system and simply transmitted as-is.

-spec motion(Body::term()) -> motion().
motion(Body) ->
    motion(#{}, Body).

-spec motion(Header::header(), Body::body()) -> motion().
motion(Header, Body) when is_map(Header) ->
    #{envelope => #{header => Header, body => Body}}.

-spec envelope(Motion::motion()) -> envelope().
envelope(Motion) ->
    #{envelope := Envelope} = Motion,
    Envelope.

-spec header(Motion::motion()) -> header().
header(Motion) ->
    Envelope = envelope(Motion),
    #{header := Header} = Envelope,
    Header.

-spec body(Motion::motion()) -> body().
body(Motion) ->
    Envelope = envelope(Motion),
    #{body := Body} = Envelope,
    Body.

-spec command(Body::body()) -> 
                     motion(). 
command(Body) ->
    command(#{}, Body).

-spec command(Header::header(), Body::body()) -> 
                     motion().
command(Header, Body) ->
    motion(Header#{type => command}, Body).

-spec document(Body::body()) ->
                      motion(). 
document(Body) ->
    document(#{}, Body).

-spec document(Header::header(), Body::body()) -> 
                      motion().
document(Header, Body) ->
    motion(Header#{type => document}, Body).

-spec event(Body::body()) -> 
                   motion(). 
event(Body) ->
    event(#{}, Body).

-spec event(Header::header(), Body::body()) -> 
                   motion().
event(Header, Body) ->
    motion(Header#{type => event}, Body).

-spec request_reply(Body::body(), Address::term()) -> 
                           motion().
request_reply(Body, Address) ->
    request_reply(#{}, Body, Address).

-spec request_reply(Header::header(), Body::body(), Address::term()) -> 
                           motion().
request_reply(Header, Body, Address) ->
    motion(Header#{type => request_reply, return_address => Address}, Body).

-spec request_reply(Header::header(), Body::body(), Address::term(), Ref::reference()) -> 
                           motion().
request_reply(Header, Body, Address, Ref) ->
    motion(Header#{type => request_reply, return_address => Address, reference => Ref}, Body).

%% API.

%% That next statement will be produced by system itself: erlmachine_system:damage(Assembly, Damage);
%% Transmission can provide a lot of abilities, for example:
%% Time measurements between parts, different flow algorithms inside gearbox etc..
%% Actually, it's just tree , and we'll be able to do that by various ways;
%% We can even provide slowering between parts or persistence layer, because control level was provided;
%% Error handling will be implemented by product API parts instead;
%% In generally term transmission is about processing algorithms over mechanical topology;

-spec start_link() -> {ok, pid()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, [], []).

%% gen_server.

init([]) ->
	{ok, #state{}}.

handle_call(_Request, _From, State) ->
	{reply, ignored, State}.

handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

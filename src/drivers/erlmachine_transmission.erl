-module(erlmachine_transmission).
-behaviour(gen_server).
%% In comparison to the common approach with global shared table for publish/subscribe interactions:
%% 1) Each gearbox applies within it's own scope via constructed graph;
%% 2) Graph structure represents a transmission vocabulary and incomming message router;

%% TODO: Can we supply subscribe expressions like rabbitmq? #, *, etc..
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
-export([mesh/4, unmesh/3]).

-export([motion/3]).
-export([header/1, header/2]).
-export([body/1, body/2]).

-export([command/2, document/2, event/2]).

%% Transmission will be loaded directly by call where ID argument is provided; 
%% Transmission can be represented by a lot of copies where each of them is marked by unique serial number;

-include("erlmachine_factory.hrl").
-include("erlmachine_system.hrl").

-type motion() :: map(). %% envelope;

-type header() :: map().

-type body() :: term().

-export_type([motion/0, envelope/0, header/0, body/0]).

%% TODO: To make via prototype call;
-spec rotate(Assembly::assembly(), Motion::term()) -> 
                    success() | failure(term(), term()).
rotate(Assembly, Motion) -> 
    Name = erlmachine_assembly:name(Assembly),
    try
        %% There is a place where statistics can be gathered;
        %% TODO: To extract neighbor extenions list and to rotate them with result and socket as arg;
        Schema = erlmachine_assembly:schema(Assembly),
        Label = erlmachine_assembly:label(Assembly),
        Exts = [Ext|| {_, Ext} <- erlmachine_schema:out_edges(Schema, Label)],
        [Name:rotate(Assembly, Motion, Ext) || Ext <- Exts],
        erlmachine:success()
    catch E:R ->
            erlmachine:failure(E, R)
    end.

%% Transmit has designed to be synchronous. It can be used for direct API calls on the particular extension;
-spec transmit(Assembly::assembly(), Motion::term()) ->
                      term() | failure(term(), term()).
transmit(Assembly, Motion) ->
    Name = erlmachine_assembly:name(Assembly),
    try
        %% There is a place where statistics can be gathered;
        Name:transmit(Assembly, Motion)
    catch E:R ->
            erlmachine:failure(E, R) 
    end.


%% TODO: TO make via graph?
%% We can supply subscription mechanism through the graph;
-spec mesh(Assembly::assembly(), Ext::assembly()) -> 
                  success() | failure(term(), term()).
mesh(Assembly, Ext) ->
    SN = serial_no(Assembly),
    Res = (prototype_name(Part)):attach(SN, GearBox, Part, Reg, Ext),
    %% Build edge (Part -> Ext);
    erlmachine_schema:add_edge(GearBox, Label, Ext),
    Res.

-spec unmesh(Assembly::assembly(), Id::term()) -> 
                    success().
unmesh(Assembly, Id) ->
    Part = erlmachine_gearbox:find(GearBox, AssemblyLabel),
    SN = serial_no(Part),
    Res = (prototype_name(Part)):detach(SN, Assembly, Part, Id),
    %% Remove edge (Part -> Ext);
    ok = erlmachine_schema:del_path(Assembly, Label, Id),
    Res.


-record(state, {
}).

%% A message consists of two basic parts:
%% 1. Header – Information used by the messaging system that describes the data being transmitted, its origin, its destination, and so on.
%% 2. Body – The data being transmitted; generally ignored by the messaging system and simply transmitted as-is.

-spec motion(Header::header(), Body::body()) -> motion().
motion(Header, Body) ->
    Motion = #{},
    body(header(Motion, Header), Body).

-spec header(Motion::motion()) -> header().
header(Motion) ->
    maps:get(header, Motion).

-spec header(Motion::motion(), Header::header()) -> motion().
header(Motion, Header) ->
    Motion#{ header => Header }.

-spec body(Motion::motion()) -> body().
body(Motion) ->
    maps:get(body, Motion).

-spec body(Motion::motion(), Body::body()) -> motion().
body(Motion, Body) ->
    Motion#{ body => Body }.

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

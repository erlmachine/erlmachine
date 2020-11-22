-module(erlmachine_transmission).
-behaviour(gen_server).
%% NOTE: The most popular approach to solve publish/subscribe interactions is about how to use shared hash table;
%% This table is filled by routes and can be accesed within whole application with appropriate tags (keys);
%% NOTE: The transmission based design:
%% 1) Each gearbox has topology with it's own exectution context and routes vocabulary;
%% 2) The data structure for incomming messages routes is based on graph;
%% 3) The transmission has responsibility to manage the schema;

%% TODO: To supply topology visualization in admin panel (errors, message history, throughput);
%% TODO: To supply message history in headers:
%% https://www.enterpriseintegrationpatterns.com/patterns/messaging/MessageHistory.html;

%% API.
-export([start_link/0]).

%% gen_server.
-export([
         init/1, 
         handle_call/3, handle_cast/2, handle_info/2, 
         terminate/2,
         code_change/3
        ]).

-export([start/1, start/2]).

-export([install/2, uninstall/2]).
-export([rotate/2, transmit/2]).

-export([motion/2]).
-export([header/1, header/2]).
-export([body/1, body/2]).

-export([stop/1]).

-include("erlmachine_assembly.hrl").
-include("erlmachine_system.hrl").

-type motion() :: map(). %% envelope;

-type header() :: map().

-type body() :: term().

-export_type([motion/0, header/0, body/0]).

%% NOTE: To operate only via schema;
%% Schema has to be extracted and managed by independent way;
%% To be able to manage topology you have to store it;

-spec start(Assembly::assembly()) -> 
                   success(pid()) | failure(term(), term()).
start(Assembly) ->
    Schema = erlmachine_schema:new(),

    start(Schema, Assembly).

-spec start(Schema::term(), Assembly::assembly()) ->
                     success(pid()) | failure(term(), term()).
start(Schema, Assembly) ->
    Rel = erlmachine_assembly:schema(Assembly, Schema),

    Name = erlmachine_assembly:name(Rel),
    try
        {ok, Pid} = Name:start(Rel), true = is_pid(Pid),
        schema(Schema, Rel),
        erlmachine:success(Pid)
    catch E:R ->
            erlmachine:failure(E, R)
    end.

-spec install(Assembly::assembly(), Ext::assembly()) -> 
                     success(pid()) | failure(term(), term()).
install(Assembly, Ext) ->
    install(Assembly, Ext, []).

-spec install(Assembly::assembly(), Ext::assembly(), Label::term()) -> 
                     success(pid()) | failure(term(), term()).
install(Assembly, Ext, Label) ->
    Schema = erlmachine_assembly:schema(Assembly),

    Name = erlmachine_assembly:name(Assembly),
    Vertex = erlmachine_assembly:label(Assembly),
    try
        {ok, Pid} = Name:install(Assembly, Ext), true = is_pid(Pid),
        erlmachine_schema:add_edge(Schema, Vertex, Ext, Label),
        erlmachine:success(Pid)
    catch E:R ->
            erlmachine:failure(E, R)
    end.

-spec uninstall(Assembly::assembly(), Vertex::term()) -> 
                       success().
uninstall(Assembly, Vertex) ->
    Schema = erlmachine_assembly:schema(Assembly),

    Name = erlmachine_assembly:name(Assembly),
    ok = Name:uninstall(Assembly, Vertex),
    ok = erlmachine_schema:del_vertex(Schema, Vertex).

%% TODO: To make via prototype call;
-spec rotate(Assembly::assembly(), Motion::term()) -> 
                    success() | failure(term(), term()).
rotate(Assembly, Motion) -> 
    Schema = erlmachine_assembly:schema(Assembly),

    Name = erlmachine_assembly:name(Assembly),
    try
        %% There is a place where statistics can be gathered;
        %% TODO: To extract neighbor extenions list and to rotate them by passing the result through socket;
        Vertex = erlmachine_assembly:label(Assembly),
        Exts = [Ext|| {_, Ext} <- erlmachine_schema:out_edges(Schema, Vertex)],
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

-spec stop(Assembly::assembly()) ->
                       success().
stop(Assembly) ->
    Schema = erlmachine_assembly:schema(Assembly),

    Name = erlmachine_assembly:name(Assembly),
    Vertex = erlmachine_assembly:label(Assembly),
    ok = Name:stop(Assembly),
    ok = erlmachine_schema:del_vertex(Schema, Vertex).

%% TODO: To supply mesh/unmesh calls with Edge label args;

-record(state, {
}).

%% A message consists of two basic parts:
%% 1. Header – Information used by the messaging system that describes the data being transmitted, its origin, its destination, and so on.
%% 2. Body – The data being transmitted; generally ignored by the messaging system and simply transmitted as-is.

%% NOTE: See the message construction patterns:
%% https://www.enterpriseintegrationpatterns.com/patterns/messaging/MessageConstructionIntro.html

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

%%%===================================================================
%%% schema access
%%%===================================================================

-spec schema(Schema::term(), Assembly::assembly()) -> term().
schema(Schema, Assembly) ->
    V = erlmachine_schema:add_vertex(Schema, Assembly),
    Exts = erlmachine_assembly:extensions(Assembly),
    schema(Schema, V, Exts).

-spec schema(Schema::term(), Label::term(), Exts::list(assembly())) -> 
                  term().
schema(Schema, _Label, []) ->
    Schema;
schema(Schema, Label, [Assembly|T]) ->
    Exts = erlmachine_assembly:extensions(Assembly),
    schema(Schema, erlmachine_schema:add_edge(Schema, Label, Assembly), Exts),
    schema(Schema, Label, T).

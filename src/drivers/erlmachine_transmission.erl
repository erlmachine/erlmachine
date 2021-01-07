-module(erlmachine_transmission).
-behaviour(gen_server).
%% NOTE: The most popular approach to solve publish/subscribe interactions is about how to use shared hash table;
%% This table is filled by routes and can be accesed within whole application with appropriate tags (keys);
%% NOTE: The transmission based design:
%% 1) Each gearbox has topology with it's own exectution context and vocabulary of routes;
%% 2) The data storage which contains routes for incomming messages is based on graph;
%% 3) The responsibility to manage the schema belongs to the transmission;
%% 4) It supports "dead letter" and "error message" queues;
%% 5) It monitors the quality of service through "mesh" call;


%% TODO: To supply cluster topology visualization in admin panel (errors, message history, throughput);
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

-export([boot/1, boot/2]).

-export([install/3, uninstall/3]).
-export([process/3]).
-export([execute/3]).

-export([shutdown/4]).

-export([mesh/3, pass/3]).

-export([motion/2]).
-export([header/1, header/2]).
-export([body/1, body/2]).

-export([spec/1]).

-include("erlmachine_assembly.hrl").
-include("erlmachine_system.hrl").

-type schema() :: erlmachine_schema:schema().
-type vertex() :: erlmachine_schema:vertex().

-type motion() :: map(). %% envelope;
-type header() :: map().
-type body() :: term().

-type spec() :: map().

-export_type([motion/0, header/0, body/0]).

%% NOTE: To operate only via schema;
%% Schema has to be extracted and managed by independent way;
%% To be able to manage topology you have to store it;

%% TODO: To decouple schema from assembly;
%% TODO: To pass schema arg each time when transmission invoked;
%% TODO: To mark each schema edge after extension is running;

-spec boot(Schema::schema(), V::term()) ->
                  success(pid()) | failure(term(), term()).
boot(Schema, V) ->
    Assembly = erlmachine_schema:vertex(Schema, V), boot(Assembly).

-spec boot(Assembly::assembly()) ->
                   success(pid()) | failure(term(), term()).
boot(Assembly) ->
    Supervisor = erlmachine:is_supervisor(Assembly), Name = erlmachine_assembly:name(Assembly),
    Res =
        if Supervisor ->
                Schema = erlmachine_assembly:schema(Assembly), V = erlmachine_assembly:vertex(Assembly),
                Exts = erlmachine_schema:out_neighbours(Schema, V),
                Name:boot(Assembly, Exts);
           true ->
                Name:boot(Assembly)
        end,
    ok = erlmachine_system:boot(Res, Assembly),
    Res.

%% NOTE: Schema and env params are inherited through thq all gearbox;
-spec spec(Assembly::assembly()) -> spec().
spec(Assembly) ->
    ID = erlmachine_assembly:vertex(Assembly),

    Name = erlmachine_assembly:name(Assembly), Type = Name:type(),
    Start = {?MODULE, boot, [Assembly]},
    #{ id => ID, start => Start, type => Type }.

-spec install(Schema::schema(), V::vertex(), Ext::assembly()) ->
                     success(pid()) | failure(term(), term()).
install(Schema, V, Ext) ->
    Assembly = erlmachine_schema:vertex(Schema, V), Name = erlmachine_assembly:name(Assembly),
    erlmachine_schema:add_vertex(Schema, erlmachine_assembly:vertex(Ext), Ext),

    Res = Name:install(Assembly, Ext),
    ok = erlmachine_system:boot(Res, Assembly, Ext),
    Res.

-spec uninstall(Schema::schema(), V::vertex(), ID::term()) ->
                       success().
uninstall(Schema, V, ID) ->
    Assembly = erlmachine_schema:vertex(Schema, V), Name = erlmachine_assembly:name(Assembly),
    Res = Name:uninstall(Assembly, ID),

    ok = erlmachine_schema:del_vertex(Schema, ID),
    ok = erlmachine_system:shutdown(Res, Assembly, ID),
    Res.

%% TODO: To make via prototype call;
-spec process(Schema::schema(), V::vertex(), Motion::term()) ->
                    success().
process(Schema, V, Motion) ->
    Assembly = erlmachine_schema:vertex(Schema, V), ok = process(Assembly, Motion).

-spec process(Assembly::assembly(), Motion::term()) ->
                     success().
process(Assembly, Motion) ->
    Name = erlmachine_assembly:name(Assembly),
    ok = Name:process(Assembly, Motion).

-spec mesh(Module::atom(), Assembly::assembly(), Motion::term()) ->
                  success(assembly()) | failure(term(), term(), assembly()).
mesh(Module, Assembly, Motion) ->
    Schema = erlmachine_assembly:schema(Assembly), V = erlmachine_assembly:vertex(Assembly),
    Exts = erlmachine_schema:out_neighbours(Schema, V),
    if Exts == [] ->
            erlmachine:success(Assembly);
       true ->
            mesh(Exts, Module, Assembly, Motion)
    end.

mesh([Ext|Range], Module, Assembly, Motion) ->
    Res = Module:mesh(Assembly, Motion, Ext, Range), ok = transmit(Res, Ext),
    ok = erlmachine_system:process(Res, Ext),
    if Range == [] ->
            Res;
       true ->
            Rel = rel(Res),
            mesh(Range, Module, Rel, Motion)
    end.

-spec pass(Module::atom(), Assembly::assembly(), Motion::term()) ->
                  success(assembly()) | failure(term(), term(), assembly()).
pass(Module, Assembly, Motion) ->
    Schema = erlmachine_assembly:schema(Assembly), V = erlmachine_assembly:vertex(Assembly),
    Exts = erlmachine_schema:out_neighbours(Schema, V),
    pass(Exts, Module, Assembly, Motion).

pass(Exts, Module, Assembly, Motion) ->
    Res = Module:pass(Assembly, Motion),
    [begin ok = transmit(Res, Ext), ok = erlmachine_system:process(Res, Ext) end|| Ext <- Exts],
    Res.

transmit({ok, _Assembly}, _Ext) ->
    ok;
transmit({ok, Ret, _Assembly}, Ext) ->
    ok = process(Ext, Ret);
transmit({error, {_E, _R}, _Assembly}, _Ext) ->
    ok.

rel({ok, Assembly}) ->
    Assembly;
rel({ok, _Ret, Assembly}) ->
    Assembly;
rel({error, {_E, _R}, Assembly}) ->
    Assembly.

%% Serve has designed to be synchronous. It can be used for direct API calls on the particular extension;
-spec execute(Schema::schema(), V::vertex(), Command::term()) ->
                      term().
execute(Schema, V, Command) ->
    Assembly = erlmachine_schema:vertex(Schema, V), Name = erlmachine_assembly:name(Assembly),
    Name:execute(Assembly, Command).

-spec shutdown(Schema::schema(), V::vertex(), Reason::term(), Timeout::term()) ->
                       success().
shutdown(Schema, V, Reason, Timeout) ->
    Assembly = erlmachine_schema:vertex(Schema, V), Name = erlmachine_assembly:name(Assembly),
    Res = Name:shutdown(Assembly, Reason, Timeout),
    ok = erlmachine_system:shutdown(Res, Assembly, V),
    Res.

%% TODO: To supply connect/disconnect by edge label args (via graph API the all interaction through schema);
%% NOTE: They must be graphically displayed;

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

id() ->
    ?MODULE.

-spec start_link() -> {ok, pid()}.
start_link() ->
    gen_server:start_link({local, id()}, ?MODULE, [], []).

%%%===================================================================
%%%  gen_server behaviour
%%%===================================================================

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


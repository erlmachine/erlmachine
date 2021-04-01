-module(erlmachine_transmission).
-behaviour(gen_server).
%% NOTE: In general the concept of a transmission describes how to build processing algorithms which are executed via structured mechanical extensions;

%% NOTE: The responsibility to manage the graph belongs to the transmission.
%% 1. Graph should be preserved and to be managed by the owner;
%% 2. Env is assigned to the Model (in comparison to the options param which is assigned to the extension);

%% NOTE: Transmission can provide developer with a various features such as:

%% a) Time measurements between mechanical extensions;
%% b) Ability to implement advanced flow algorithms (throughput capacity control, load balancing, etc..);

%% NOTE: The erlmachine based design is different.

%% The most typical implementation of publish/subscribe interchange is based on a shared hash table.
%% This table is filled by routes and can be accesed within application via appropriate tags (keys);
%% The erlmachine based message router assumes:

%% a) Each transmission has it's own internal graph which serves as a routes vocabulary;
%% b) The data storage which contains routes for incomming messages which is based on graph data structure;

%% TODO: 1. Support "dead letter" and "error message" queues;
%%       2. Monitor the quality of a service through "mesh" call;
%%       3. Display graphically on a canvas the all cextensions from a graph (appearence on startup, activity);
%%       4. Represent statistics report via dashboards (errors, message history, throughput);
%%       5. Support of message history via headers:
%%       6. https://www.enterpriseintegrationpatterns.com/patterns/messaging/MessageHistory.html

%% API.
-export([start_link/0]).

-export([motion/2]).
-export([header/1, header/2]).
-export([body/1, body/2]).

-export([startup/1, startup/2]).

-export([install/3, uninstall/3]).

-export([process/3]).
-export([execute/3]).

-export([shutdown/4]).

%% gen_server.
-export([
         init/1,
         handle_call/3, handle_cast/2, handle_info/2, 
         terminate/2,
         code_change/3
        ]).

-export([mesh/3, pass/3]).

-export([spec/1]).

-include("erlmachine_assembly.hrl").
-include("erlmachine_system.hrl").

-type graph() :: erlmachine_schema:graph().
-type vertex() :: erlmachine_schema:vertex().

-type motion() :: map(). %% envelope;
-type header() :: map().
-type body() :: term().

%% https://www.enterpriseintegrationpatterns.com/patterns/messaging/MessageConstructionIntro.html

-type spec() :: map().

-export_type([motion/0, header/0, body/0]).

%%% Graph mapping

-spec add_vertex(Assembly::assembly()) -> success().
add_vertex(Assembly) ->
    Graph = erlmachine_assembly:graph(Assembly), V = erlmachine_assembly:vertex(Assembly),
    Rel = erlmachine_assembly:extensions(Assembly, []),

    erlmachine_graph:add_vertex(Graph, V, Rel).

-spec add_edge(Assembly, Ext, Label) -> success().
add_edge(Assembly, Ext, Label) ->
    Graph = erlmachine_assembly:graph(Assembly),
    V1 = erlmachine_assembly:vertex(Assembly), V2 = erlmachine_assembly:vertex(Ext),

    erlmachine_graph:add_edge(Graph, V1, V2, Label).

id() ->
    ?MODULE.

-spec start_link() -> {ok, pid()}.
start_link() ->
    gen_server:start_link({local, id()}, ?MODULE, [], []).

-spec startup(Assembly::assembly(), Env::map()) ->
                  success(pid()) | failure(term(), term()).
startup(Assembly, Env) ->
    Rel = erlmachine_assembly:env(Assembly, Env),
    startup(Rel).

%% NOTE: Schema and env params are inherited through the whole transmission;
-spec spec(Assembly::assembly()) -> spec().
spec(Assembly) ->
    ID = erlmachine_assembly:vertex(Assembly), Type = erlmachine_assembly:type(Assembly),

    Start = { ?MODULE, startup, [Assembly] },
    #{ id => ID, start => Start, type => Type }.

-spec startup(Assembly::assembly()) ->
                   success(pid()) | failure(term(), term()).
startup(Assembly) ->
    Env = erlmachine_assembly:env(Assembly),
    Exts = [erlmachine_assembly:env(Ext, Env)|| Ext <- erlmachine_assembly:extensions(Assembly)],

    IsSup = erlmachine:is_supervisor(Assembly),
    Res =
        if IsSup ->
                erlmachine_supervisor_prototype:startup(Assembly, Exts);
           true ->
                erlmachine_worker_prototype:startup(Assembly)
        end,

    Graph = erlmachine_assembly:graph(Assembly),

    ok = add_vertex(Assembly), [ok = add_vertex(erlmachine_assembly:graph(Ext, Graph))|| Ext <- Exts],
    [ok = add_edge(Assembly, Ext, []) || Ext <- Exts],

    ok = erlmachine_system:startup(Res, Assembly), 
    Res.

-spec install(Graph::graph(), V::vertex(), Ext::assembly()) ->
                     success(pid()) | failure(term(), term()).
install(Graph, V, Ext) ->
    %% TODO To add extensions on a graph
    Assembly = erlmachine_graph:vertex(Graph, V), Env = erlmachine_assembly:env(Assembly),

    Rel = erlmachine_assembly:env(Ext, Env),
    Res = erlmachine_supervisor_prototype:install(Assembly, Rel),

    ok = add_vertex(erlmachine_assembly:graph(Rel, Graph)), ok = add_edge(Assembly, Rel, []),

    ok = erlmachine_system:install(Res, Assembly, Rel),
    Res.

-spec uninstall(Graph::graph(), V::vertex(), ID::term()) ->
                       success().
uninstall(Graph, V, ID) ->
    Assembly = erlmachine_graph:vertex(Graph, V),

    Res = erlmachine_supervisor_prototype:uninstall(Assembly, ID),

    ok = erlmachine_graph:del_vertex(Graph, ID),

    ok = erlmachine_system:uninstall(Res, Assembly, ID),
    Res.

-spec process(Graph::graph(), V::vertex(), Motion::term()) ->
                    success().
process(Graph, V, Motion) ->
    Assembly = erlmachine_graph:vertex(Graph, V), ok = process(Assembly, Motion).

-spec process(Assembly::assembly(), Motion::term()) ->
                     success().
process(Assembly, Motion) ->
    ok = erlmachine_worker_prototype:process(Assembly, Motion).

-spec mesh(Module::atom(), Assembly::assembly(), Motion::term()) ->
                  success(assembly()) | failure(term(), term(), assembly()).
mesh(Module, Assembly, Motion) ->
    Schema = erlmachine_assembly:graph(Assembly), V = erlmachine_assembly:vertex(Assembly),
    Exts = erlmachine_graph:out_neighbours(Graph, V),
    if Exts == [] ->
            erlmachine:success(Assembly);
       true ->
            mesh(Exts, Module, Assembly, Motion)
    end.

mesh([Ext|Range], Module, Assembly, Motion) ->
    Res = Module:mesh(Assembly, Motion, Ext, Range),

    ok = erlmachine_system:process(Res, Ext), ok = transmit(Res, Ext),

    if Range == [] ->
            Res;
       true ->
            Rel = rel(Res),
            mesh(Range, Module, Rel, Motion)
    end.

-spec pass(Module::atom(), Assembly::assembly(), Motion::term()) ->
                  success(assembly()) | failure(term(), term(), assembly()).
pass(Module, Assembly, Motion) ->
    Schema = erlmachine_assembly:graph(Assembly), V = erlmachine_assembly:vertex(Assembly),
    Exts = erlmachine_graph:out_neighbours(Schema, V),
    pass(Exts, Module, Assembly, Motion).

pass(Exts, Module, Assembly, Motion) ->
    Res = Module:pass(Assembly, Motion),

    [begin ok = erlmachine_system:process(Res, Ext), ok = transmit(Res, Ext) end|| Ext <- Exts], Res.

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

-spec execute(Graph:graph(), V::vertex(), Command::term()) ->
                      term().
execute(Graph, V, Command) ->
    Assembly = erlmachine_graph:vertex(Graph, V),

    erlmachine_worker_prototype:execute(Assembly, Command).

-spec shutdown(Graph::graph(), V::vertex(), Reason::term(), Timeout::term()) ->
                       success().
shutdown(Graph, V, Reason, Timeout) ->
    Assembly = erlmachine_graph:vertex(Graph, V),
    Res = erlmachine_worker_prototype:shutdown(Assembly, Reason, Timeout),

    ok = erlmachine_system:shutdown(Res, Assembly, V),

    ok = erlmachine_graph:del_vertex(Graph, V), 
    Res.

-record(state, {}).

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

%% NOTE: A message consists of two basic parts:
%% 1) Header – Information used by the messaging system that describes the data being transmitted, its origin, its destination, and so on.
%% 2) Body – The data being transmitted; generally ignored by the messaging system and simply transmitted as-is.

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

-module(erlmachine_transmission).
-behaviour(gen_server).
%% NOTE: In general the concept of a transmission is about "how to build processing algorithms which are based on structured mechanical extensions";
%% NOTE: The responsibility to manage the schema belongs to the transmission.
%% 1. Schema should be preserved and to be managed by the owner;
%% 2. Env is assigned to the Model (in comparison to the options param which is assigned to the extension);

%% NOTE: Transmission can provide developer with a various features such as:

%% a) Time measurements between mechanical parts;
%% b) Implementation of advanced flow algorithms (throughput capacity control, load balancing, etc..);

%% NOTE: The most typical implementation of publish/subscribe interchange is based on shared hash table.
%% This table is filled by routes and can be accesed within whole application through appropriate tags (keys);
%% The erlmachine based design is different and assumes:

%% a) Each engine has it's own internal topology which serves as a routes vocabulary;
%% b) The data storage which contains routes for incomming messages is based on graph data structure;

%% TODO: Support "dead letter" and "error message" queues;
%% TODO: Monitor the quality of a service through "mesh" call;
%% TODO: Display graphically on a chart the all components from a schema. To animate active extensions;
%% TODO: Represent statistics report via dashboards (errors, message history, throughput);
%% TODO: Support of message history via headers:
%% https://www.enterpriseintegrationpatterns.com/patterns/messaging/MessageHistory.html;

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

-type schema() :: erlmachine_schema:schema().
-type vertex() :: erlmachine_schema:vertex().

-type motion() :: map(). %% envelope;
-type header() :: map().
-type body() :: term().

-type spec() :: map().

-export_type([motion/0, header/0, body/0]).

id() ->
    ?MODULE.

-spec start_link() -> {ok, pid()}.
start_link() ->
    gen_server:start_link({local, id()}, ?MODULE, [], []).


%% TODO To supply Env as argument
-spec startup(Schema::schema(), V::term(), Env::map()) ->
                  success(pid()) | failure(term(), term()).
startup(Schema, V, Env) ->
    Assembly = erlmachine_schema:vertex(Schema, V), startup(Assembly, Env).

-spec startup(Assembly::assembly(), Env::map()) ->
                     success(pid()) | failure(term(), term()).
startup(Assembly, Env) ->
    Rel = erlmachine_assembly:env(Assembly, Env),
    startup(Rel).

-spec startup(Assembly::assembly(), Env::map()) ->
                   success(pid()) | failure(term(), term()).
startup(Assembly, Env) ->
    Env = erlmachine_assembly:env(Assembly),

    IsSup = erlmachine:is_supervisor(Rel),
    Res =
        if IsSup ->
                Graph = erlmachine_assembly:graph(Rel), V = erlmachine_assembly:vertex(Rel),
                Exts = [ext(Ext, Env)|| Ext <- erlmachine_schema:out_neighbours(Graph, V)],
                erlmachine_supervisor_prototype:startup(Rel, Exts);
           true ->
                erlmachine_worker_prototype:startup(Rel)
        end,

    ok = erlmachine_system:startup(Res, Rel), Res.

-spec ext(Assembly::assembly(), Env::map()) -> assembly().
ext(Assembly, Env) ->
    Graph = erlmachine_assembly:graph(Assembly),
    erlmachine_assembly:graph(erlmachine_assembly:env(Ext, Env), Graph).

%% NOTE: Schema and env params are inherited through the whole transmission;
-spec spec(Assembly::assembly()) -> spec().
spec(Assembly) ->
    ID = erlmachine_assembly:vertex(Assembly),

    Type = erlmachine_assembly:type(Assembly),
    Start = { ?MODULE, startup, [Assembly] },
    #{ id => ID, start => Start, type => Type }.

-spec install(Schema::schema(), V::vertex(), Ext::assembly(), Env::map()) ->
                     success(pid()) | failure(term(), term()).
install(Schema, V, Ext, Env) ->
    Assembly = erlmachine_schema:vertex(Schema, V),
    Rel = ext(Ext, Env), 
    Res = erlmachine_supervisor_prototype:install(Assembly, Rel),

    ok = erlmachine_system:install(Res, Assembly, Ext),

    erlmachine_schema:add_vertex(Schema, erlmachine_assembly:vertex(Rel), Rel), Res.

-spec uninstall(Schema::schema(), V::vertex(), ID::term()) ->
                       success().
uninstall(Schema, V, ID) ->
    Assembly = erlmachine_schema:vertex(Schema, V),
    Res = erlmachine_supervisor_prototype:uninstall(Assembly, ID),

    ok = erlmachine_system:uninstall(Res, Assembly, ID),

    ok = erlmachine_schema:del_vertex(Schema, ID), Res.

-spec process(Schema::schema(), V::vertex(), Motion::term()) ->
                    success().
process(Schema, V, Motion) ->
    Assembly = erlmachine_schema:vertex(Schema, V), ok = process(Assembly, Motion).

-spec process(Assembly::assembly(), Motion::term()) ->
                     success().
process(Assembly, Motion) ->
    ok = erlmachine_worker_prototype:process(Assembly, Motion).

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
    Schema = erlmachine_assembly:schema(Assembly), V = erlmachine_assembly:vertex(Assembly),
    Exts = erlmachine_schema:out_neighbours(Schema, V),
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

-spec execute(Schema::schema(), V::vertex(), Command::term()) ->
                      term().
execute(Schema, V, Command) ->
    Assembly = erlmachine_schema:vertex(Schema, V),

    erlmachine_worker_prototype:execute(Assembly, Command).

-spec shutdown(Schema::schema(), V::vertex(), Reason::term(), Timeout::term()) ->
                       success().
shutdown(Schema, V, Reason, Timeout) ->
    Assembly = erlmachine_schema:vertex(Schema, V),
    Res = erlmachine_worker_prototype:shutdown(Assembly, Reason, Timeout),

    ok = erlmachine_system:shutdown(Res, Assembly, V),

    ok = erlmachine_schema:del_vertex(Schema, V), Res.

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


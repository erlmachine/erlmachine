-module(erlmachine_ct).

-behaviour(gen_server).

%% API.
-export([start/1, stop/0]).

-export([install/1, uninstall/1]).
-export([add_edge/2]).
-export([process/2]).
-export([execute/2]).
-export([shutdown/1]).
-export([pressure/2]).

%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).

-include_lib("erlbox/include/erlbox.hrl").

-include("erlmachine_factory.hrl").
-include("erlmachine_assembly.hrl").
-include("erlmachine_graph.hrl").

id() ->
    ?MODULE.

-spec start(Assembly::assembly()) -> success(pid()) | ingnore | failure(term()).
start(Assembly) ->
    gen_server:start({local, id()}, ?MODULE, Assembly, []).

-record(install, { extension::assembly() }).

-spec install(Ext::assembly()) -> success(pid()) | failure(term(), term()).
install(Ext) ->
    gen_server:call(id(), #install{ extension = Ext }).

-record(add_edge, { vertex::vertex(), vertex2::vertex() }).

-spec add_edge(V::vertex(), V2::vertex()) -> success().
add_edge(V, V2) ->
    gen_server:call(id(), #add_edge{ vertex = V, vertex2 = V2 }).

-record(execute, { vertex::vertex(), action::term() }).

-spec execute(V::vertex(), Action::term()) -> term().
execute(V, Action) ->
    gen_server:call(id(), #execute{ vertex = V, action = Action }).

-record(uninstall, { vertex::vertex() }).

-spec uninstall(V::vertex()) -> success().
uninstall(V) ->
    gen_server:call(id(), #uninstall{ vertex = V }).

-record(shutdown, { vertex::vertex() }).

-spec shutdown(V::vertex()) -> success().
shutdown(V) ->
    gen_server:call(id(), #shutdown{ vertex = V }).

-record(process, { vertex::vertex(), motion::term() }).

-spec process(V::term(), Motion::term()) -> success().
process(V, Motion) ->
    gen_server:call(id(), #process{ vertex = V, motion = Motion }).

-record(pressure, { vertex::vertex(), load::term() }).

-spec pressure(V::vertex(), Load::term()) -> success().
pressure(V, Load) ->
    erlang:send(id(), #pressure{ vertex = V, load = Load }), 
    ok.

%% TODO: To provide transmission API;

-spec stop() -> success().
stop() ->
    gen_server:stop(id()).

%%%===================================================================
%%% gen_server
%%%===================================================================

-record(state, { graph::graph(), root::vertex() }).

init(Assembly) ->
    Graph = erlmachine_graph:draw(Assembly), V = erlmachine:vertex(Assembly),
    {ok, Pid} = erlmachine:startup(Graph), true = is_pid(Pid),

    {ok, #state{ graph = Graph, root = V }}.


handle_call(#install{ extension = Ext }, _From, #state{ graph = Graph, root = V } = State) ->
    Res = erlmachine:install(Graph, V, Ext),

    {reply, Res, State};

handle_call(#add_edge{ vertex = V, vertex2 = V2 }, _From, #state{ graph = Graph } = State) ->
    Res = erlmachine_graph:add_edge(Graph, V, V2, 'test'),

    {reply, Res, State};

handle_call(#uninstall{ vertex = V2 }, _From, #state{ graph = Graph, root = V } = State) ->
    Res = erlmachine:uninstall(Graph, V, V2),

    {reply, Res, State};

handle_call(#execute{ vertex = V, action = Action }, _From, #state{ graph = Graph } = State) ->
    Res = erlmachine:execute(Graph, V, Action),

    {reply, Res, State};

handle_call(#shutdown{ vertex = V2 }, _From, #state{ graph = Graph } = State) ->
    Res = erlmachine:shutdown(Graph, V2),

    {reply, Res, State};

handle_call(#process{ vertex = V, motion = Motion }, _From, #state{ graph = Graph } = State) ->
    ok = erlmachine:process(Graph, V, Motion),

    {reply, ok, State};

handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(#pressure{ vertex = _V, load = _Load }, #state{ graph = _Graph } = State) ->
    %% TODO
    {noreply, State};

handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
    ok.

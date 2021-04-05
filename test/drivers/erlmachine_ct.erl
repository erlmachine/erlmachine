-module(erlmachine_ct).

-behaviour(gen_server).

%% API.
-export([start/2, stop/0]).

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

-include("erlmachine_factory.hrl").
-include("erlmachine_assembly.hrl").
-include("erlmachine_graph.hrl").
-include("erlmachine_system.hrl").

id() ->
    ?MODULE.

-record(start, { assembly::assembly(), env::map() }).

-spec start(Assembly::assembly(), Env::map()) -> success(pid()) | ingnore | failure(term()).
start(Assembly, Env) ->
    gen_server:start({local, id()}, ?MODULE, #start{ assembly = Assembly, env = Env }, []).

-record(install, { extension::assembly() }).

-spec install(Ext::assembly()) -> success(pid()) | failure(term(), term()).
install(Ext) ->
    gen_server:call(id(), #install{ extension = Ext }).

-record(add_edge, { vertex::vertex(), vertex2::vertex() }).

-spec add_edge(V::vertex(), V2::vertex()) -> success().
add_edge(V, V2) ->
    gen_server:call(id(), #add_edge{ vertex = V, vertex2 = V2 }).

-record(execute, { vertex::vertex(), command::term() }).

-spec execute(V::vertex(), Command::term()) -> term().
execute(V, Command) ->
    gen_server:call(id(), #execute{ vertex = V, command = Command }).

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
    gen_server:cast(id(), #process{ vertex = V, motion = Motion }).

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
%%% gen_server behaviour
%%%===================================================================

-record(state, { graph::graph(), root::vertex() }).

init(#start{ assembly = Assembly, env = Env }) ->
    Graph = erlmachine_graph:draw(Assembly), V = erlmachine:vertex(Assembly),

    {ok, Pid} = erlmachine:startup(Graph, Assembly, Env), true = is_pid(Pid),
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

handle_call(#execute{ vertex = V, command = Command }, _From, #state{ graph = Graph } = State) ->
    Res = erlmachine:execute(Graph, V, Command),

    {reply, Res, State};

handle_call(#shutdown{ vertex = V2 }, _From, #state{ graph = Graph } = State) ->
    Res = erlmachine:shutdown(Graph, V2),

    {reply, Res, State};

handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

handle_cast(#process{ vertex = V, motion = Motion }, #state{ graph = Graph } = State) ->
    ok = erlmachine:process(Graph, V, Motion),

    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(#pressure{ vertex = _V, load = _Load }, #state{ graph = _Graph } = State) ->
    %% TODO
    {noreply, State};

handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
    ok.

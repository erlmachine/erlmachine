-module(erlmachine_sample).

-behaviour(gen_server).

%% API.
-export([start/1]).
-export([install/3, uninstall/3]).
-export([stop/1]).

%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).

-include("erlmachine_factory.hrl").
-include("erlmachine_assembly.hrl").
-include("erlmachine_system.hrl").

-spec start(Assembly::assembly()) ->
                   success(pid()) | ingnore | failure(term()).
start(Assembly) ->
    gen_server:start(?MODULE, Assembly, []).

-record(install, { vertex::term(), extension::assembly() }).

-spec install(Pid::pid(), Vertex::term(), Ext::assembly()) ->
                       success(pid()) | failure(term(), term()).
install(Pid, Vertex, Ext) ->
    gen_server:call(Pid, #install{ vertex = Vertex, extension = Ext }).

-record(uninstall, { vertex::term(), id::term() }).

-spec uninstall(Pid::pid(), Vertex::term(), Id::term()) ->
                       success().
uninstall(Pid, Vertex, Id) ->
    gen_server:call(Pid, #uninstall{ vertex = Vertex, id = Id }).

%% TODO: To provide transmission API;

-spec stop(Pid::pid()) -> success().
stop(Pid) ->
    gen_server:stop(Pid).

%%%===================================================================
%%% gen_server behaviour
%%%===================================================================

-record(state, { schema::term() }).

init(Assembly) ->
    Schema = erlmachine_schema:new(),
    {ok, _Pid} = erlmachine_transmission:start(Schema, Assembly),
    {ok, #state{ schema = Schema }}.

handle_call(#install{ vertex = Vertex, extension = Ext }, _From, State) ->
    Schema = State#state.schema,
    {ok, Pid} = erlmachine:install(Schema, Vertex, Ext),

    {reply, erlmachine:success(Pid), State};

handle_call(#uninstall{ vertex = Vertex, id = Id }, _From, State) ->
    Schema = State#state.schema,
    ok = erlmachine:uninstall(Schema, Vertex, Id),
    {reply, erlmachine:success(), State};

handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
    ok.

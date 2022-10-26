-module(erlmachine_system).
%% TODO: To provide automated system monitoring within predefined range of tags: system, log, etc.

-behaviour(erlmachine_scope).
-behaviour(gen_server).

-export([scope/0]).

-export([start_link/0]).

-export([init/1]).
-export([handle_call/3, handle_cast/2, handle_info/2]).
-export([terminate/2]).

-export([startup/2]).
-export([install/3]).
-export([transmit/1, transmit/2]).
-export([shutdown/3]).
-export([uninstall/3]).

-include_lib("erlbox/include/erlbox.hrl").

-include("erlmachine_assembly.hrl").
-include("erlmachine_factory.hrl").

%%% erlmachine_scope

-spec scope() -> atom().
scope() ->
    ?MODULE.

%% API

name() ->
    ?MODULE.

-spec start_link() -> {ok, pid()}.
start_link() ->
    Name = name(),
    gen_server:start_link({local, Name}, ?MODULE, [], []).

%%% gen_server

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

%%% Transmission hub

%% TODO:
%% 1. Supply restarts counter is an edge;
%% 2. To manage the overall extension vocabulary via syn;
%% 3. To deliver control messages ("overlad", etc.) via syn
%% 4. To manage process exit via syn (syn_event_handler)

-spec startup(success(Pid::pid()) | failure(E::term(), R::term()), Assembly::assembly()) ->
                     success().
startup({ok, Pid}, Assembly) when is_pid(Pid) ->
    _Graph = erlmachine:graph(Assembly),
    Rel = erlmachine:tag(Assembly, {'pid', Pid}),

    ok = erlmachine_scope:join(?MODULE, _Group = ?MODULE, Pid, Rel),
    %ok = add_vertex(Schema, Rel),
    ok;
startup({error, {E, R}}, Assembly) ->
    _Graph = erlmachine:graph(Assembly),

    _Rel = erlmachine:tag(Assembly, {'error', {E, R}}),
    %ok = add_vertex(Schema, Rel),
    ok.

-spec install(success(Pid::pid()) | failure(E::term(), R::term()), Assembly::assembly(), Ext::assembly()) ->
                     success().
install(_Res = {ok, Pid}, Assembly, Ext) when is_pid(Pid) ->
    _Graph = erlmachine:graph(Assembly),

    Rel = erlmachine:tag(Assembly, {'pid', Pid}),
    %ok = add_vertex(Schema, Rel),

    _V1 = erlmachine:vertex(Rel), _V2 = erlmachine:vertex(Ext),

    ok = erlmachine_scope:join(?MODULE, _Group = ?MODULE, Pid, Ext),
    %erlmachine_schema:add_edge(Schema, V1, V2, Res),
    ok;
install(_Res = {error, {E, R}}, Assembly, Ext) ->
    _Graph = erlmachine:graph(Assembly),

    _Rel = erlmachine:tag(Assembly, {'error', {E, R}}),
    %ok = add_vertex(Schema, Rel),

    _V1 = erlmachine:vertex(Assembly), _V2 = erlmachine:vertex(Ext),
    %erlmachine_schema:add_edge(Schema, V1, V2, Res),

    ok.

transmit({ok, _Assembly}) ->
    %% TODO: Place for vertex statistics gathering;
    ok;
transmit({ok, _Ret, _Assembly}) ->
    ok;
transmit({error, {E, R}, Assembly}) ->
    _Graph = erlmachine:graph(Assembly),

    _Rel = erlmachine:tag(Assembly, {'error', {E, R}}),
    %ok = add_vertex(Schema, Rel),

    %% NOTE: We should support DB indexing based on vertexes and edges;
    %% TODO: To create a separate in-memory datastore to store errors and invocations;
    ok.

transmit({ok, _Assembly}, _Ext) ->
    ok;
transmit({ok, _Ret, _Assembly}, _Ext) ->
    %% TODO: Place for edge statistics gathering;
    ok;
transmit(_Res = {error, {E, R}, Assembly}, Ext) ->
    _Graph = erlmachine:graph(Assembly),

    _Rel = erlmachine:tag(Assembly, {'error', {E, R}}),
    %ok = add_vertex(Schema, Rel),

    _V1 = erlmachine:vertex(Assembly), _V2 = erlmachine:vertex(Ext),
    %erlmachine_schema:add_edge(Schema, V1, V2, Res),

    %% TODO: Place for errors statistics gathering (we can mark edge by red color);
    ok.


uninstall(_, _Assembly, _V) ->
    ok.

shutdown(ok, _Assembly, _V) ->
    ok.

%-spec add_vertex(Schema::term(), Assembly::assembly()) -> success().
%add_vertex(_Schema, _Assembly) ->
   % Rel = erlmachine_assembly:extensions(Assembly, []),
    %erlmachine_schema:add_vertex(Schema, Assembly),
 %   ok.

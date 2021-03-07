-module(erlmachine_system).
%% TODO: To provide automated system monitoring within predefined range of tags: system, log, etc.

-behaviour(erlmachine_registry).

-export([group/0]).

-export([failure/0, failure/1, failure/2, failure/3]).
-export([success/0, success/1, success/2]).

-export([startup/2]).
-export([install/3]).
-export([process/1, process/2]).
-export([shutdown/3]).

-export([is_success/1, is_failure/1]).

-include("erlmachine_assembly.hrl").
-include("erlmachine_factory.hrl").

-type failure() :: error.
-type failure(E) :: {error, E}.
-type failure(E, R) :: {error, {E, R}}.
-type failure(E, R, State) :: {error, {E, R}, State}.
-type success(Result) :: {ok, Result}.
-type success(Result, State) :: {ok, Result, State}.
-type success() :: ok.

-export_type([failure/1, failure/2, failure/3, success/0, success/1, success/2]).

%%% erlmachine_registry

-spec group() -> atom().
group() ->
    ?MODULE.

-spec failure() -> failure().
failure() ->
    error.

-spec failure(E::term()) -> failure(term()).
failure(E) ->
    {error, E}.

-spec failure(E::term(), R::term()) ->
                     failure(E::term(), R::term()).
failure(E, R) ->
    {error, {E, R}}.

-spec failure(E::term(), R::term(), S::term()) -> failure(term(), term(), term()).
failure(E, R, S) ->
    {error, {E, R}, S}.

-spec success(Res::term()) -> success(term()).
success(Res) ->
    {ok, Res}.

-spec success(Res::term(), S::term()) -> success(term(), term()).
success(Res, S) ->
    {ok, Res, S}.

-spec success() -> success().
success() ->
    ok.

is_success(ok) ->
    true;
is_success({ok, _}) ->
    true;
is_success({ok, _, _}) ->
    true;
is_success(_) ->
    false.

-spec is_failure(term()) -> boolean().
is_failure(error) ->
    true;
is_failure({error, {_, _}}) ->
    true;
is_failure({error, {_, _}, _}) ->
    true;
is_failure(_) ->
    false.

%%% Transmission hub

%% TODO:
%% 1. Supply restarts counter is an edge;
%% 2. To manage the overall extension vocabulary via syn;
%% 3. To deliver control messages ("overlad", etc.) via syn
%% 4. To manage process exit via syn (syn_event_handler) 

-spec startup(success(Pid::pid()) | failure(E::term(), R::term()), Assembly::assembly()) -> 
                     success().
startup({ok, Pid}, Assembly) when is_pid(Pid) ->
    _Schema = erlmachine_assembly:schema(Assembly),
    _Rel = erlmachine_tag:pid(Assembly, Pid),

    ok = erlmachine_registry:join(?MODULE, Pid, Assembly),
    %ok = add_vertex(Schema, Rel),
    ok;
startup({error, {E, R}}, Assembly) ->
    _Schema = erlmachine_assembly:schema(Assembly),

    _Rel = erlmachine_tag:error(Assembly, E, R),
    %ok = add_vertex(Schema, Rel), 
    ok.

-spec install(success(Pid::pid()) | failure(E::term(), R::term()), Assembly::assembly(), Ext::assembly()) ->
                     success().
install(_Res = {ok, Pid}, Assembly, Ext) when is_pid(Pid) ->
    _Schema = erlmachine_assembly:schema(Assembly),

    _Rel = erlmachine_tag:pid(Assembly, Pid),
    %ok = add_vertex(Schema, Rel),

    _V1 = erlmachine_assembly:vertex(Assembly), _V2 = erlmachine_assembly:vertex(Ext),

    ok = erlmachine_registry:join(?MODULE, Pid, Ext),
    %erlmachine_schema:add_edge(Schema, V1, V2, Res),
    ok;
install(_Res = {error, {E, R}}, Assembly, Ext) ->
    _Schema = erlmachine_assembly:schema(Assembly),

    _Rel = erlmachine_tag:error(Assembly, E, R),
    %ok = add_vertex(Schema, Rel),

    _V1 = erlmachine_assembly:vertex(Assembly), _V2 = erlmachine_assembly:vertex(Ext),
    %erlmachine_schema:add_edge(Schema, V1, V2, Res),

    ok.

-spec transmit()
process({ok, _Assembly}) ->
    %% TODO: Place for vertex statistics gathering;
    ok;
process({ok, _Ret, _Assembly}) ->
    ok;
process({error, {E, R}, Assembly}) ->
    _Schema = erlmachine_assembly:schema(Assembly),

    _Rel = erlmachine_tag:error(Assembly, E, R),
    %ok = add_vertex(Schema, Rel),

    %% NOTE: We should support DB indexing based on vertexes and edges;
    %% TODO: To create a separate in-memory datastore to store errors and invocations;

    ok.

process({ok, _Assembly}, _Ext) ->
    ok;
process({ok, _Ret, _Assembly}, _Ext) ->
    %% TODO: Place for edge statistics gathering;
    ok;
process(_Res = {error, {E, R}, Assembly}, Ext) ->
    _Schema = erlmachine_assembly:schema(Assembly),

    _Rel = erlmachine_tag:error(Assembly, E, R),
    %ok = add_vertex(Schema, Rel),

    _V1 = erlmachine_assembly:vertex(Assembly), _V2 = erlmachine_assembly:vertex(Ext),
    %erlmachine_schema:add_edge(Schema, V1, V2, Res),

    %% TODO: Place for errors statistics gathering (we can mark edge by red color);
    ok.

uninstall(Res, Assembly, ID)

shutdown(ok, _Assembly, _V) ->
    ok.

%-spec add_vertex(Schema::term(), Assembly::assembly()) -> success().
%add_vertex(_Schema, _Assembly) ->
   % Rel = erlmachine_assembly:extensions(Assembly, []),
    %erlmachine_schema:add_vertex(Schema, Assembly),
 %   ok.


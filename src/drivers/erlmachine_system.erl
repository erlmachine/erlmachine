-module(erlmachine_system).

-export([failure/0, failure/1, failure/2, failure/3]).
-export([success/0, success/1, success/2]).

-export([boot/2, boot/3]).
-export([process/1, process/2]).
-export([shutdown/3]).

-export([is_success/1, is_failure/1]).

-include("erlmachine_factory.hrl").

-type failure() :: error.
-type failure(E) :: {error, E}.
-type failure(E, R) :: {error, {E, R}}.
-type failure(E, R, State) :: {error, {E, R}, State}.
-type success(Result) :: {ok, Result}.
-type success(Result, State) :: {ok, Result, State}.
-type success() :: ok.

-export_type([failure/1, failure/2, failure/3, success/0, success/1, success/2]).

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

%%%===================================================================
%%% Transmission hub
%%%===================================================================

boot({ok, Pid}, Assembly) when is_pid(Pid) ->
    Schema = erlmachine_assembly:schema(Assembly),

    Rel = erlmachine_tag:pid(Assembly, Pid),
    erlmachine_schema:add_vertex(Schema, Rel),
    ok;
boot({error, {E, R}}, Assembly) ->
    Schema = erlmachine_assembly:schema(Assembly),

    Rel = erlmachine_tag:error(Assembly, E, R),
    erlmachine_schema:add_vertex(Schema, Rel),
    ok.

boot(Res = {ok, Pid}, Assembly, Ext) when is_pid(Pid) ->
    Schema = erlmachine_assembly:schema(Assembly),

    Rel = erlmachine_tag:pid(Assembly, Pid),
    erlmachine_schema:add_vertex(Schema, Rel),

    V1 = erlmachine_assembly:label(Assembly), V2 = erlmachine_assembly:label(Ext),
    erlmachine_schema:add_edge(Schema, V1, V2, Res),

    ok;
boot(Res = {error, {E, R}}, Assembly, Ext) ->
    Schema = erlmachine_assembly:schema(Assembly),

    Rel = erlmachine_tag:error(Assembly, E, R),
    erlmachine_schema:add_vertex(Schema, Rel),

    V1 = erlmachine_assembly:label(Assembly), V2 = erlmachine_assembly:label(Ext),
    erlmachine_schema:add_edge(Schema, V1, V2, Res),

    ok.

process({ok, _Assembly}) ->
    %% TODO: Place for vertex statistics gathering;
    ok;
process({ok, _Ret, _Assembly}) ->
    ok;
process({error, {E, R}, Assembly}) ->
    Schema = erlmachine_assembly:schema(Assembly),

    Rel = erlmachine_tag:error(Assembly, E, R),
    erlmachine_schema:add_vertex(Schema, Rel),

    %% NOTE: We should support DB indexing based on vertexes and edges;
    %% TODO: To create a separate in-memory datastore to store errors and invocations;

    ok.

process({ok, _Assembly}, _Ext) ->
    ok;
process({ok, _Ret, _Assembly}, _Ext) ->
    %% TODO: Place for edge statistics gathering;
    ok;
process(Res = {error, {_E, _R}, Assembly}, Ext) ->
    Schema = erlmachine_assembly:schema(Assembly),

    V1 = erlmachine_assembly:label(Assembly), V2 = erlmachine_assembly:label(Ext),
    erlmachine_schema:add_edge(Schema, V1, V2, Res),

    %% TODO: Place for errors statistics gathering (we can mark edge by red color);
    ok.

shutdown(ok, _Assembly, _V) ->
    ok.





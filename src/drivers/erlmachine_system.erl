 -module(erlmachine_system).

-export([failure/0, failure/1, failure/2, failure/3]).
-export([success/0, success/1, success/2]).

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

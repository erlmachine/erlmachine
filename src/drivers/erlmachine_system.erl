 -module(erlmachine_system).
-behaviour(gen_server).

%% API.
-export([start_link/0]).

%% gen_server.
-export([
         init/1,
         handle_call/3, handle_cast/2, handle_info/2, 
         terminate/2,
         code_change/3
        ]).

-export([failure/1, failure/2, failure/3]).
-export([success/0, success/1, success/2]).

-export([overtime/3]).

-include("erlmachine_factory.hrl").

-type failure(E, R) :: {error, {E, R}}.
-type failure(E) :: {error, E}.
-type failure(E, R, State) :: {error, {E, R}, State}.
-type success(Result) :: {ok, Result}.
-type success(Result, State) :: {ok, Result, State}.
-type success() :: ok.

-export_type([failure/1, failure/2, failure/3, success/0, success/1, success/2]).

-spec failure(E::term(), R::term()) -> failure(E::term(), R::term()).
failure(E, R) -> 
    {error, {E, R}}.

-spec failure(E::term()) -> failure(E::term()).
failure(E) ->
    {error, E}.

-spec failure(E::term(), R::term(), State::term()) -> failure(E::term(), R::term(), State::term()).
failure(E, R, State) -> 
    {error, {E, R}, State}.

-spec success(Result::term()) -> success(Result::term()).
success(Result) ->
    {ok, Result}.

-spec success(Result::term(), State::term()) -> success(Result::term(), State::term()).
success(Result, State) -> 
    {ok, Result, State}.

-spec success() -> success().
success() ->
    ok.

-record(state, {
}).

%% API.

-spec start_link() -> {ok, pid()}.
start_link() ->
	gen_server:start_link(?MODULE, [], []).

%% gen_server.

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

-spec overtime(Assembly::assembly(), Station::station(), Throughput::integer()) -> 
                      ok.
overtime(_Assembly, _Station, _Throughput) ->
    ok.
 
%% crash, damage, block, overtime, production_limit callbacks will be provided; 

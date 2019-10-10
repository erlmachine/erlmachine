-module(erlmachine_assembly_station).
-behaviour(gen_server).

%% API.
-export([start_link/0]).

%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-spec steps(Module::atom()) -> list(atom()).
steps(Module) ->
    erlmachine:attribute(Module, steps, []).

-record(station, {name::atom(), steps::list(atom()), throughput::throughput()}).

-type station()::station().

-spec station(Name::atom()) -> Station::station().
station(Name) ->  
    Module = Name, 
    #station{name=Name, steps=steps(Name)}.

-spec pipe(Station::station()) ->  

(Assembly::assembly(), Stations::list(station())) ->
    success(assembly()) | failure(term(), term(), assembly()) | failure(term()).

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

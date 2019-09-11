-module(shaft_base_prototype).

-behaviour(gen_server).
-behaviour(erlmachine_assembly).
-behaviour(shaft_prototype).

%% API.
-export([start_link/0]).

%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-record(state, {}).

%% API.

-spec install() -> {ok, pid()}.
install() ->
    gen_server:start_link(?MODULE, [], []).

start_repair() ->
     ok.

stop_repair() ->
     ok.

-spec uninstall() -> ok.
uninstall(ServerRef) ->
    gen_server:stop().

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

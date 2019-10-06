-module(erlmachine_warehouse).
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

%% We need to provide broken part issue callback for reason which is oposite to normal;
%% Correct processing of issue will be provided by factory;
%% It's need to be sent to warehouse;
%% It's a not the same like standard unmount;
%% The specialized "crash" callback from factory will be provided;
%% From warehouse perspective are two key's message exist - produced and broken;
%% I am going to store rejected parts on warehouse;
%% We need to provide the specialized callback for that;

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

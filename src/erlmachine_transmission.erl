-module(erlmachine_transmission).
-behaviour(gen_server).

%% API.
-export([start_link/0]).
-export([load/2]).

%% Transmission will be loaded directly by call where ID argument is provided. Transmission can have a lot of copies where each of them is marked by unique serial number

%% Callbacks
 
%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-record(state, {
}).

%% API.

-spec start_link() -> {ok, pid()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, [], []).

-spec load(ID::term(), Force::term()) -> Force.
load(ID, Force) -> 
    erlang:send(ID, Force).

-spec output(ID::term(), Force::term()) -> Motion::term().
output(ID, Force) ->
    output(ID, Force, 5000).

output(ID, Force, TimeOut) ->
    erlang:send(ID, Force), %% TODO implement sender notify via envelop
    receive
        Motion ->
            Motion
    after
        TimeOut ->
            throw(timeout)
    end.

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

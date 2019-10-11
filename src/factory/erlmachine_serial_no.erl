-module(erlmachine_serial_no).
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

-type serial_no()::binary().

-export_type([serial_no/0]).

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

%% generate a readable string representation of a SN.
%%
%% base64url encoding was provided; 
%% This format is safer and more applicable by web (in comparison with base64);

base64url(In) ->
    lists:reverse(lists:foldl(fun ($\+, Acc) -> [$\- | Acc];
                                  ($\/, Acc) -> [$\_ | Acc];
                                  ($\=, Acc) -> Acc;
                                  (Chr, Acc) -> [Chr | Acc]
                              end, [], base64:encode_to_string(In))).

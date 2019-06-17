-module(erlmachine_tracker).
-behaviour(gen_server).

%% API.
-export([start_link/0]).
-export([start_tracking/1, stop_tracking/1]).
-export([get_catalogue/0]).

%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-callback tracking_id(Packakge::map()) -> ID::binary().
-callback tracking_info(Package::map()) -> Info::binary().

-optional_callbacks([tracking_info/1]).

%% API.

-record('tracking.start', {package :: map()}).
-record('tracking.stop', {package :: map()}).
-record('tracking.catalogue', {}).

-spec start_link() -> {ok, pid()}.
start_link() ->
	gen_server:start_link(?MODULE, [], []).

-spec start_tracking(Package::map()) -> {ok, ID::binary()} | {error, Reason::term()}.
start_tracking(Package) ->
    gen_server:call(?MODULE, #'tracking.start'{package = Package}).

-spec stop_tracking(Package::map()) -> ok | {error, Reason::term()}.
stop_tracking(Package) ->
    gen_server:call(?MODULE, #'tracking.stop'{package = Package}).

-spec get_catalogue() -> {ok, List::list()} | {error, Reason::term()}.
get_catalogue() ->
    gen_server:call(?MODULE, #'tracking.catalogue'{}).

%% gen_server.

-record(state, {catalogue = #{} :: map()}).

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

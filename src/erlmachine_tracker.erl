-module(erlmachine_tracker).
-behaviour(gen_server).

%% API.
-export([start_link/0]).
-export([start_tracking/1, track/1, stop_tracking/1]).
-export([catalogue/0]).

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

-record('start', {package :: map()}).
-record('stop', {package :: map()}).
-record('catalogue', {filter = <<"*">> :: binary()}).
-record('track', {package :: map()}).

-spec start_link() -> {ok, pid()}.
start_link() ->
    EmptyCatalog = #{},
    start_link(EmptyCatalog).

-spec start_link(Catalog::map()) -> {ok, pid()}.
start_link(Catalog) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Catalog, []).

-spec start_tracking(Package::map()) -> {ok, ID::binary()} | {error, Reason::term()}.
start_tracking(Package) ->
    gen_server:call(?MODULE, #'start'{package = Package}).

-spec stop_tracking(Package::map()) -> {ok, ID::binary()} | {error, Reason::term()}.
stop_tracking(Package) ->
    gen_server:call(?MODULE, #'stop'{package = Package}).

-spec catalogue() -> {ok, List::list()} | {error, Reason::term()}.
catalogue() ->
    catalogue(<<"*">>).

-spec catalogue(Filter::binary()) -> {ok, List::list()} | {error, Reason::term()}.
catalogue(Filter) ->
    gen_server:call(?MODULE, #'catalogue'{filter = Filter}).

-spec track(Package::map()) -> {ok, ID::binary()} | {error, Reason::term()}.
track(Package) ->
     gen_server:call(?MODULE, #'track'{package = Package}).

%% gen_server.

-record(state, {catalogue = #{} :: map()}).

init(Catalogue) ->
	{ok, #state{catalogue = Catalogue}}.

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

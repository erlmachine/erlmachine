-module(erlmachine_tracker).

-folder(<<"erlmachine/erlmachine_tracker">>).

-behaviour(gen_server).
-behaviour(erlmachine_transmission).

%% API.
-export([start_link/1]).
-export([tracking_number/2, trace/2]).

%% Callbacks

%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

%% erlmachine_filesystem
-export([directory/0, directory/3]).


-callback tag(Packakge::term()) -> ID::binary().

%% API.

-record('trace', {package :: map(), tracking_number :: binary()}).

-spec start_link(Catalog::map()) -> {ok, pid()}.
start_link(Catalog) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Catalog, []).

-spec tracking_number(Tracker::atom(), Package::map()) -> Number::binary().
tracking_number(Tracker, Package) ->
    ID = Tracker:tracking_id(Package), %% TODO implement a composition with GUID
    GUID = <<"GUID">>, %% TODO 
    <<ID/binary, ".", GUID/binary>>.

-spec trace(TrackingNumber::binary(), Package::map()) -> TrackingNumber::binary().
trace(TrackingNumber, Package) ->
     erlang:send(?MODULE, #'trace'{package = Package, tracking_number = TrackingNumber}).


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


%% erlmachine_catalogue.


%% erlmachine_transmission.



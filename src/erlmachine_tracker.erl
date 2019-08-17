-module(erlmachine_tracker).

-behaviour(gen_server).
-behaviour(erlmachine_catalogue).
-behaviour(erlmachine_transmission).

%% API.
-export([start_link/0]).
-export([tracking_number/2, trace/2]).

%% Callbacks

%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

%% erlmachine_catalogue
-export([directory/0, directory/3]).


-callback tag(Packakge::term()) -> ID::binary().

%% API.

-record('trace', {package :: map(), tracking_number :: binary()}).
-record('directory', {path :: binary(), user :: user(), options :: list()}).

-spec start_link() -> {ok, pid()}.
start_link() ->
    EmptyCatalog = #{},
    start_link(EmptyCatalog).

-spec start_link(Catalog::map()) -> {ok, pid()}.
start_link(Catalog) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Catalog, []).

-spec tracking_number(Tracker::atom(), Package::map()) -> Number::binary().
tracking_number(Tracker, Package) ->
    ID = Tracker:tracking_id(Package), %% TODO implement a composition with GUID
    GUID = <<"GUID">>, %% TODO 
    <<ID/binary, ".", GUID/binary>>.

-spec folder(Path::binary(), User::user(), Options::list()) -> {ok, Files::list()} | {error, Reason::term()}.
folder(Path, User, Options) ->
    erlang:send(?MODULE, #'directory'{path = Path, user = User, options = Options}),
    %% TODO execute the direct function call here
    [].

-spec trace(TrackingNumber::binary(), Package::map()) -> TrackingNumber::binary().
trace(TrackingNumber, Package) ->
     erlang:send(?MODULE, #'trace'{package = Package, tracking_number = TrackingNumber}).

-spec shape() -> ok.
shape() ->
     ok.



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



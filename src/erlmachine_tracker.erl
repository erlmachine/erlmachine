-module(erlmachine_tracker).

-folder(<<"erlmachine/erlmachine_tracker">>).

-input([?MODULE]). %% output([?MODULE]).

-behaviour(gen_server).

%% API.
-export([start_link/0]).
-export([tracking_no/1, tracking_no/2, trace/2]).

%% Callbacks

%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-include("erlmachine_factory.hrl").
-include("erlmachine_system.hrl").

-callback tag(Packakge::term()) -> Tag::binary().

%% API.

-spec start_link() -> success(pid()) | ignore | failure(term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec tracking_no(Tracker::atom(), Package::term()) -> success(tracking_no()) | failure(term(), term()).
tracking_no(Tracker, Package) ->
    Tag = Tracker:tag(Package),
    tracking_no(Tag).

-spec tracking_no(Tag::binary()) -> success(tracking_no()) | failure(term(), term()).
tracking_number(Tag) when is_binary(Tag) ->
    GUID = <<"GUID">>, %% TODO 
    <<Tag/binary, ".", GUID/binary>>.

-record(trace, {package::map(), tracking_no::binary()}).

-spec trace(TrackingNumber::binary(), Package::map()) -> success(tracking_no()) | failure(term(), term()).
trace(TrackingNumber, Package) ->
    erlang:send(?MODULE, #trace{tracking_no = TrackingNumber, package = Package}).


%% gen_server.

-record(accept, {}).
-record(state, {gearbox::assembly(), file::file_id()}).

init([]) ->
    GearBox = erlmachine_factory:gearbox(?MODULE, []), 
    %% We need to implement storing gearboxes and also individual part inside warehouse;
    %% After that we will be able to select needed parts by SN;
    FileId = erlmachine_file:create(<<"./process.report">>),
    {ok,  #state{gearbox = GearBox, file_id = FileId}, {continue, #accept{}}}.

handle_call(_Request, _From, State) ->
    %% We need to provide REST API for management inside transmission
    %% We need to incapsulate transmission management inside callbacks
    %% We need to provide  measurements of transmission loading, etc..
	{reply, ignored, State}.

handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info(#trace{} = Command, #state{gearbox = GearBox} = State) ->
    #trace{tracking_no = TrackingNumber, package = Package} = Command,
    erlmachine_transmission:rotate(GearBox, #{TrackingNumber => Package}), %% we need to find default input here
    {noreply, State};
handle_info() ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%% We consider Module as implementation point (like class) and serial number as instance - (like object); 
%% We can support polymorphism by different ways - by overriding prototype or by changing topology itself;
handle_continue(#accept{}, State#state{gearbox = GearBox, file_id = FileId}) ->
    try
        erlmachine_factory:accept(GearBox),
        SN = erlmachine_assembly:serial_no(GearBox),
        erlmachine_file:write(FileId, #{accept => #{serial_no => SN}}),
        {noreply, State};
    catch E:R ->
            erlmachine_file:write(FileId, #{accept => #{error => E, reason => R}}),
            {stop, {E, R}, State}
    end;
handle_continue(_, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

format_status(Opt, [PDict, State]) ->
    [].



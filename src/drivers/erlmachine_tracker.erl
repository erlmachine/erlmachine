-module(erlmachine_tracker).

-folder(<<"erlmachine/erlmachine_tracker">>).

-input([?MODULE]).

-behaviour(gen_server).

-export([tabname/0, record_name/0, attributes/0]).

%% API.
-export([start_link/0]).

%% gen_server.
-export([
         init/1, 
         handle_call/3, handle_cast/2, handle_info/2,
         handle_continue/2,
         terminate/2,
         code_change/3,
         format_status/2
        ]).

-export([tracking_no/1, tracking_no/2]).
-export([to_track/2, track/1]).

-include("erlmachine_factory.hrl").
-include("erlmachine_system.hrl").

-callback tag(Packakge::term()) -> Tag::binary().

-record(track, { tracking_no::binary(), package::map() }).

-type serial() :: erlmachine_serial:serial().

-type tracking_no()::binary().

-export_type([tracking_no/0]).

%% I guess tracker needs to be optimized by gen_batch_server modification;
%% As a table is located completely on a disc 
%% the specialized event for the journal purpouses needs to be provided too;

%% API.

-spec tabname() -> atom().
tabname() ->
    ?MODULE.

-spec record_name() -> atom().
record_name() ->
    track.

-spec attributes() -> list(atom()).
attributes() ->
    record_info(fields, track).

id() -> 
    ?MODULE.

-spec start_link() ->
                        success(pid()) | ignore | failure(term()).
start_link() ->
    Id = id(),
    gen_server:start_link({local, Id}, ?MODULE, [], []).

-spec tracking_no(Tracker::atom(), Package::term()) -> 
                         success(tracking_no()) | failure(term(), term()).
tracking_no(Tracker, Package) ->
    Tag = Tracker:tag(Package),
    tracking_no(Tag).

-spec tracking_no(Tag::binary()) -> 
                         success(tracking_no()) | failure(term(), term()).
tracking_no(Tag) when is_binary(Tag) ->
    try 
        TN = tracking_no(),
        erlmachine:success(<<Tag/binary, ".", TN/binary>>)
    catch E:R ->
            erlmachine:failure(E, R)
    end.

-record (tracking_no, {}).

-spec tracking_no() -> tracking_no().
tracking_no() ->
    Id = id(),
    TN = gen_server:call(Id, #tracking_no{}),
    erlmachine:base64url(TN).

%% Additional options like mode, etc. can be added later;

-spec to_track(TN::binary(), Package::map()) -> 
                   success().
to_track(TN, Package) ->
    ok = mnesia:dirty_write(tabname(), #track{ tracking_no=TN, package=Package }).

-spec track(TN::binary()) -> 
                   success(list()) | failure(term(), term()).
track(TN) ->
    Result = mnesia:dirty_read(tabname(), TN),
    {ok, Result}.

%% gen_server.

-record(accept, { }).
-record(state, { gearbox::assembly(), serial::serial(), tracking_no::serial_no() }).

init([]) ->
    %% TODO the special notification gear for journal needs to be implemented;

    {ok, Serial} = erlmachine_serial:tracking_no(),
    TN = erlmachine_serial_no:serial_no(Serial),

    {ok,  #state{ serial=Serial, tracking_no=TN }, {continue, #accept{}}}.

handle_call(#tracking_no{}, _From, #state{ serial=Serial, tracking_no=TN }=State) ->

    Inc = erlmachine_serial:inc(Serial),

    Rotate = erlmachine_serial_no:serial_no(Inc, TN),
    {reply, TN, State#state{ serial=Inc, tracking_no=Rotate }};

handle_call(_Request, _From, State) ->
    %% We need to provide REST API for management inside transmission
    %% We need to incapsulate transmission management inside callbacks
    %% We need to provide  measurements of transmission loading, etc..
    {reply, ignored, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Message, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%% We consider Module as implementation point (like class) and serial number as instance - (like object); 
%% We can support polymorphism by different ways - by overriding prototype or by changing topology itself;
handle_continue(#accept{}, #state{gearbox=_GearBox}=State) ->
    try
        %% true = erlmachine_factory:accept(GearBox),
        {noreply, State}
    catch E:R ->
            {stop, {E, R}, State}
    end;
handle_continue(_, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

format_status(_Opt, [_PDict, _State]) ->
    [].


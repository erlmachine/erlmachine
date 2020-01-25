-module(erlmachine_tracker).

-folder(<<"erlmachine/erlmachine_tracker">>).

-input([?MODULE]).

-behaviour(gen_server).

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
-export([track/2]).

-export([record_name/0, attributes/0]).

-include("erlmachine_factory.hrl").
-include("erlmachine_system.hrl").

-callback tag(Packakge::term()) -> Tag::binary().

-record(track, { tracking_no::binary(), package::map() }).

-type tracking_no()::binary().

-export_type([tracking_no/0]).

%% I guess tracker needs to be optimized by gen_batch_server modification;
%% As a table is located completely on a disc 
%% the specialized event for the journal purpouses needs to be provided too;

-spec record_name() -> atom().
record_name() ->
    track.

-spec attributes() -> list(atom()).
attributes() ->
    record_info(fields, track).

%% API.

-spec id() -> atom().
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

-spec track(TN::binary(), Package::map()) -> 
                   success().
track(TN, Package) ->
    Id = id(),
    erlang:send(Id, #track{ tracking_no=TN, package=Package }), 
    ok.

%% gen_server.

-record(accept, { }).
-record(state, { tracking_no::serial_no() }).

init([]) ->
    %% TODO the special notification gear for journal needs to be implemented;
    {ok, Serial} = erlmachine_serial:update(?MODULE),
    TN = erlmachine_serial_no:serial_no(Serial),

    {ok, #state{ tracking_no=TN }, {continue, #accept{}}}.

handle_call(#tracking_no{}, _From, #state{ tracking_no=TN }=State) ->
    {ok, Serial} = erlmachine_serial:update(?MODULE),
    Rotate = erlmachine_serial_no:serial_no(Serial, TN),

    {reply, TN, State#state{ tracking_no=Rotate }};

handle_call(_Request, _From, State) ->
    %% We need to provide REST API for management inside transmission
    %% We need to incapsulate transmission management inside callbacks
    %% We need to provide  measurements of transmission loading, etc..
    {reply, ignored, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(#track{ tracking_no=TN, package=Package }, State) ->
    %% TODO erlmachine itself can be configured to work еhrough ELK;
    io:format("~n~p~n~p~n~p~n",[?MODULE, TN, Package]),
    {noreply, State};

handle_info(_Message, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%% We consider Module as implementation point (like class) and serial number as instance - (like object); 
%% We can support polymorphism by different ways - by overriding prototype or by changing topology itself;
handle_continue(#accept{}, #state{}=State) ->
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


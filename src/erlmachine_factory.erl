-module(erlmachine_factory).

-folder(<<"erlmachine/factory">>).
-file(<<"erlmachine_factory.serial">>).

-behaviour(gen_server).

%% API.
-export([start_link/0]).

%% We assume that factory will also provide production of all components and their registration too;
%% My assumption that is factory can be driven from production capacity perspective; 
%% Measurements over manufactures production activity needs to be provided too;

%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-include("erlmachine_factory.hrl").
-include("erlmachine_filesystem.hrl").

-record(conveyor, {assembly::assembly(), passed=[]::list(station()), stations=[]::list(atom())}).

-type conveyor()::#conveyor{}.

-export_type([conveyor/0]).
%% Here are different kind of builders can be provided;
%% For example - YAML builder;
%% But from begining we are going to build directly from code;

%% The main purpouse of the factory is to provide product planing abilities;
%% We can control available capacity of all individual parts;
%% We can utilize different pools for that purpouse;
%% The all managment over thoose capabilities is a warehouse option;

-spec сonveyor(Assembly::assembly(), Stations::list(station())) -> 
                      success(Release::assembly()) | failure(term(), term(), Reject::assembly()).


-spec pass(Conveyor::conveyor()) -> conveyor().
pass(Conveyor) ->
    #conveyor{names=Names}=Conveyor,
    Pass = pipe(Conveyor#conveyor{names=[?MODULE|Names]}),
    %% At that point we can store Pass information and provide research over this data;
    Pass.

-spec pipe(Conveyor::conveyor()) -> Pipe::conveyor().
pipe(#conveyor{stations=Stations}=Conveyor) ->
    BuildStations = [erlnachine_assembly_station:station(Name) || Name <- Stations],
    Pipe =
        lists:foldl(
          fun(Station, #conveyor{assembly=Assembly, passed=Passed}=Conveyor) ->
                  PassStation = erlmachine_assembly_station:pass(Station, Assembly),
                  Release = erlmachine_assembly_station:output(Result),
                  Conveyor#conveyor{assembly=Release, passed=[PassStation|Passed]}
          end,
          Conveyor,
          BuildStations
         ),
    #conveyor{passed=Passed} = Pipe,
    Pipe#conveyor{passed=lists:reverse(Passed)}.

%% API.

-spec serial_no(Prefix::binary()) -> SN::binary().
serial_no(Perfix) ->
    SN = serial_no(),
    <<Prefix/binary, "-", SN/binary>>.

-record(serial_no, {}).

-spec serial_no() -> SN::binary().
serial_no() ->
    %% Just default timeout for the first time;
    ID = id(),
    SN = gen_server:call(ID, #serial_no{}),
    SN.

id() -> 
    {local, ?MODULE}.

-spec start_link() -> 
                        success(pid()) | ingnore | failure(E::term()).
start_link() ->
    ID = id(),
    gen_server:start_link(ID, ?MODULE, [], []).

%% gen_server.

-record(state, {serial::integer(), no::no()}).

init([]) ->
    Serial = erlmachine:read_serial(?MODULE), N = erlmachine_serial_no:no(Serial),
    {ok, #state{serial=Serial, no=No}}.

handle_call(#serial_no{}, _From, #{serial=Serial, no=No}=State) ->
    SN = erlmachine_serial_no:serial_no(No),
    IncSerial = erlmachine:serial(Serial),
    RotateNo = erlmachine_serial_no:no(No, IncSerial),
    {reply, SN, State#state{serial=IncSerial, no=RotateNo}};

handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

handle_cast(_Msg, State) ->	{noreply, State}.

handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, #state{serial=Serial}=State) ->
    ok = erlmachine:write_serial(?MODULE, Serial),
    ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

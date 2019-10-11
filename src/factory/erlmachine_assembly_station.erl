-module(erlmachine_assembly_station).
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

-export([steps/1]).

-export([station/1]).

-export([throughput/1, throughput/2]).

-spec steps(Module::atom()) -> list(atom()).
steps(Module) ->
    Steps = erlmachine:attribute(Module, steps, []),
    Steps.

pass(Station) ->
    Name = name(Station), Steps = steps(Station),
    Start = erlang:system_time(),
    Output = lists:foldl(fun (Step, Input) -> Name:Step(Input) end, Input, Steps),
    Stop = erlang:system_time(),
    Station#station{input=Input, throughput=Stop-Start, output=Output}.

-record(station, {
                  name::atom(),
                  input::term(),
                  steps::list(step()),
                  output::term(),
                  throughput::throughput()
                 }).

-type station()::station().

-spec name(Station::station()) -> term().
name(Station) ->
    Station#station.name.

-spec input(Station::station()) -> term().
input(Station) ->
    Station#station.input.

-spec input(Station::station(), Input::term()) -> station().
input(Station, Input) ->
    Station#station{input=Input}.

-spec station(Station::station(), Name::atom()) -> station().
station(Station, Name) ->  
    Module = Name, 
    #station{name=Name, steps=steps(Name)}.

-spec steps(Station::station()) -> list(atom()).
output(Station) ->
    Station#station.steps.

-spec output(Station::station()) -> term().
output(Station) ->
    Station#station.output.

-spec output(Station::station(), Output::term()) -> station().
output(Station, Output) ->
    Station#station{output=Output}.

-spec throughput(Station::station()) -> term().
throughput(Station) ->
    Station#station.throughput.

-spec throughput(Station::station(), Throughput::term()) -> station().
throughput(Station, Throughput) ->
    Station#station{throughput=Throughput}.

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

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

-export([station/1, station/2, pass/1]).

-export([
         name/1, 
         input/1, input/2, 
         output/1, output/2, 
         throughput/1, throughput/2
        ]).

-record(station, {
                  name::atom(),
                  input::term(),
                  steps::list(step()),
                  passed::list(step()),
                  output::term(),
                  throughput::throughput()
                 }).

-type station()::station().

-type step()::atom().
-type throughput()::integer().

-export_type([station/0, step/0, throughput/0]).

-spec steps(Module::atom()) -> list(atom()).
steps(Module) ->
    Steps = erlmachine:attribute(Module, steps, []),
    Steps.

-spec pass(Station::station(), Input::term()) -> station().
pass(Station, Input) ->
    pass(input(Station, Input)).

-spec pass(Station::station()) -> station().
pass(Station) ->
    %% The reason why we provided measurements is ability to determinate:
    %% How much is spend of time for acceptance test;
    %% How much time is spend for custom listed stations;
    %% How much time is spend for install;
    %% How much time is spend for SN allocation; etc.
    #station{name=Name, steps=Steps, input=Input} = Station,
    Start = erlang:system_time(), Output = pipe(Station), Stop = erlang:system_time(),
    Station#station{throughput=Stop-Start, output=Output}.

-spec pipe(Station::station()) -> Pipe::station().
pipe(#station{name=Name, input=Input, steps=Steps}=Station) ->
    BuildSteps = [{Name, Step}|| Step <- Steps],
    Pipe = 
        lists:foldl(
          fun({Name, Step}, #station{input=Input, passed=Passed}=Station) ->
                  Output = Name:Step(Input),
                  Station#station{output=Output, passed=[Step|Passed]}
          end, 
          Station,
          BuildSteps
         ),
    #station{passed=Passed} = Pipe,
    Pipe#station{passed=lists:reverse(Passed)}.

-spec station(Name::atom()) -> Station::station().
station(Name) ->
    Station = #station{name=Name, steps=steps(Name)},
    Station.

-spec station(Name::atom(), Load::term()) -> station().
station(Name, Load) ->  
    Station = station(Name),
    input(Station, Load).

-spec input(Station::station()) -> term().
input(Station) ->
    Station#station.input.

-spec input(Station::station(), Input::term()) -> station().
input(Station, Input) ->
    Station#station{input=Input}.

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

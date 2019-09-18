-module(erlmachine_transmission).
-behaviour(gen_server).

%% API.
-export([start_link/0]).
-export([load/2]).

%% Transmission will be loaded directly by call where ID argument is provided. Transmission can have a lot of copies where each of them is marked by unique serial number
%% I guess erlmachine_factory will be able to provide convenient way for elements registration (for example {via, syn, <<"your process name">>}) and motion method which is applicable to that registry
%% I guess each module can determinate motion


%% Gear trains with two gears[edit]
%% The simplest example of a gear train has two gears. The "input gear" (also known as drive gear) transmits power to the "output gear" (also known as driven gear). The input gear will typically be connected to a power source, such as a motor or engine. In such an example, the output of torque and rotational speed from the output (driven) gear depend on the ratio of the dimensions of the two gears.
%% Transmission doesn't take any obligations for synchronous calls, it is all about responsibility of caller (only API is provided);

%% Callbacks
 
%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-record(state, {
}).

%% API.

-spec start_link() -> {ok, pid()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, [], []).

-spec output(ID::term(), Force::term()) -> Motion::term().
output(ID, Force) ->
    output(ID, Force, 5000).

output(ID, Force, TimeOut) ->
    erlang:send(ID, Force), %% TODO implement sender notify via envelop
    receive
        Motion ->
            Motion
    after
        TimeOut ->
            throw(timeout)
    end.

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

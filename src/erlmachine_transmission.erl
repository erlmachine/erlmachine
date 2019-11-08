-module(erlmachine_transmission).
-behaviour(gen_server).

%% API.
-export([start_link/0]).
 
%% gen_server.
-export([
         init/1, 
         handle_call/3, handle_cast/2, handle_info/2, 
         terminate/2,
         code_change/3
        ]).

-export([rotate/2, transmit/3, attach/2, detach/2]).

%% Transmission will be loaded directly by call where ID argument is provided; 
%% Transmission can be represented by a lot of copies where each of them is marked by unique serial number;

-include("erlmachine_factory.hrl").
-include("erlmachine_system.hrl").

-spec rotate(Assembly::assembly(), Motion::term()) ->
                    Motion::term().
rotate(Assembly, Motion) ->
    SN = erlmachine_assembly:serial_no(Assembly),
    (erlmachine_assembly:prototype_name(Assembly)):rotate(SN, Motion).

-spec transmit(Assembly::assembly(), Motion::term(), TimeOut::integer()) ->
                      success(term()) | failure(term(), term()).
transmit(Assembly, Motion, TimeOut) ->
    SN = erlmachine_assembly:serial_no(Assembly),
    (erlmachine_assembly:prototype_name(Assembly)):transmit(SN, Motion, TimeOut).

-spec attach(Assembly::assembly(), Part::assembly()) -> 
                   success(term()) | failure(term(), term()).
attach(Assembly, Part) ->
    SN = erlmachine_assembly:serial_no(Assembly),
    (erlmachine_assembly:prototype_name(Assembly)):attach(SN, Part).

-spec detach(Assembly::assembly(), ID::serial_no()) -> 
                     success(term()) | failure(term(), term()).
detach(Assembly, Part) ->
    SN = erlmachine_assembly:serial_no(Assembly), ID = erlmachine_assembly:serial_no(Part),
    (erlmachine_assembly:prototype_name(Assembly)):detach(SN, ID).

-record(state, {
}).

%% API.

%% That next statement will be produced by system itself: erlmachine_system:damage(Assembly, Damage);
%% Transmission can provide a lot of abilities, for example:
%% Time measurements between parts, different flow algorithms inside gearbox etc..
%% Actually, it's just tree , and we'll be able to do that by various ways;
%% We can even provide slowering between parts or persistence layer, because control level was provided;
%% Error handling will be implemented by product API parts instead;
%% In generally term transmission is about processing algorithms over mechanical topology;

-spec start_link() -> {ok, pid()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, [], []).

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

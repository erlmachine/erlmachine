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

%% Transmission will be loaded directly by call where ID argument is provided; 
%% Transmission can be represented by a lot of copies where each of them is marked by unique serial number;

-export([attach/3, rotate/2]).

-export([switch_model/3, attach_model/3, detach_model/3, rotate_model/3, transmit_model/4]).

-export([switched/3, attached/3, detached/3]).

-include("erlmachine_factory.hrl").
-include("erlmachine_system.hrl").


-callback attach(SN::serial_no(), ID::serial_no(), Body::term()) -> 
    success(term()) | failure(term(), term(), term()) | failure(term()).

-callback detach(SN::serial_no(), ID::serial_no(), Body::term()) -> 
    success(term()) | failure(term(), term(), term()) | failure(term()).

-callback rotate(SN::serial_no(), Motion::term(), Body::term()) -> 
    success(term()) | failure(term(), term(), term()) | failure(term()).

-callback transmit(SN::serial_no(), Motion::term(), Body::term()) -> 
    success(term(), term()) | failure(term(), term(), term()) | failure(term()).

-spec attach(Assembly::assembly(), Part::assembly()) -> 
                    success(term()) | failure(term(), term(), term()) | failure(term()).
attach(Assembly::assembly(), Part::assembly()) ->
    Module = erlmachine_assembly:prototype_name(Assembly),
    SN = erlmachine_assembly:serial_no(Assembly),
    

-spec rotate(Assembly::assembly(), Motion::term()) ->
                    Motion::term().
rotate(Assembly, Motion) ->
    Module = erlmachine_assembly:prototype_name(Assembly),
    SN = erlmachine_assembly:serial_no(Assembly),
    Module:rotate(SN, Motion).

-spec attach_model(Assembly::assembly(), Part::assembly(), Body::term()) ->
                          success(term()) | failure(term(), term(), term()) | failure(term()).
attach_model(Assembly, Part, Body) ->
    Module = erlmachine_assembly:model_name(Assembly),
    SN = erlmachine_assembly:serial_no(Assembly),
    ID = erlmachine_assembly:serial_no(Part),
    Module:attach(SN, ID, Body).

-spec detach_model(Assembly::assembly(), Part::assembly(), Body::term()) ->
                          success(term()) | failure(term(), term(), term()) | failure(term()).
detach_model(Assembly, Part, Body) ->
    Module = erlmachine_assembly:model_name(Assembly),
    SN = erlmachine_assembly:serial_no(Assembly),
    ID = erlmachine_assembly:serial_no(Part),
    Module:detach(SN, ID, Body).

-spec rotate_model(Assembly::assembly(), Motion::term(), Body::term()) ->
                          success(term()) | failure(term(), term(), term()) | failure(term()).
rotate_model(Assembly, Motion, Body) ->
    Module = erlmachine_assembly:model_name(Assembly),
    SN = erlmachine_assembly:serial_no(Assembly),
    Module:rotate(SN, Motion, Body).

-spec transmit_model(Assembly::assembly(), Part::assembly(), Motion::term(), Body::term()) ->
                          success(term(), term()) | failure(term(), term(), term()) | failure(term()).
transmit_model(_Assembly, Part, Motion, Body) ->
    Module = erlmachine_assembly:model_name(Part),
    SN = erlmachine_assembly:serial_no(Part),
    Module:transmit(SN, Motion, Body).

-spec switched(Assembly::assembly(), Part::assembly(), Extension::assembly()) ->
                      ok.
switched(Assembly, Part, Extension) ->
    Module = erlmachine_assembly:prototype_name(Assembly),
    SN = erlmachine_assembly:serial_no(Assembly),
    Module:switched(SN, Assembly, Part, Extension).

-spec attached(Assembly::assembly(), Part::assembly(), Extension::assembly()) ->
                      ok.
attached(Assembly, Part, Extension) ->
    Module = erlmachine_assembly:prototype_name(Assembly),
    SN = erlmachine_assembly:serial_no(Assembly),
    Module:attached(SN, Assembly, Part, Extension).

-spec detached(Assembly::assembly(), Part::assembly(), Extension::assembly()) ->
                      ok.
detached(Assembly, Part, Extension) ->
    Module = erlmachine_assembly:prototype_name(Assembly),
    SN = erlmachine_assembly:serial_no(Assembly),
    Module:detached(SN, Assembly, Part, Extension).

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

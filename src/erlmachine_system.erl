-module(erlmachine_system).
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

-export([overload_model/3, block_model/4]).

-export([overloaded/3, blocked/4

-record(state, {
}).

-spec overload_model(Assembly::assembly(), Load::term(), Body::term()) ->
                            success(term()) | failure(term(), term()) | failure(term()).
overload_model(Assembly, Load, Body) ->
    Module = erlmachine_assembly:model_name(Assembly),
    SN = erlmachine_assembly:serial_no(Assembly),
    Module:overload(SN, Load, Body).

-spec block_model(Assembly::assembly(), Part::term(), Failure::failure(), Body::term()) ->
                         success(term()) | failure(term(), term()) | failure(term()).
block_model(Assembly, Part, Failure, Body) ->
    Module = erlmachine_assembly:model_name(Assembly),
    SN = erlmachine_assembly:serial_no(Assembly),
    ID = erlmachine_assembly:serial_no(Part),
    Module:block(SN, ID, Failure, Body).

-spec overloaded(Assembly::assembly(), Part::assembly(), Load::term()) ->
                        ok.
overloaded(Assembly, Part, Load) ->
    Module = erlmachine_assembly:prototype_name(Assembly),
    SN = erlmachine_assembly:serial_no(Assembly),
    Module:overloaded(SN, Assembly, Part, Load).

-spec blocked(Assembly::assembly(), Part::assembly(), Extension::assembly(), Failure::failure()) ->
                     ok.
blocked(Assembly, Part, Extension, Failure) ->
    Module = erlmachine_assembly:prototype_name(Assembly),
    SN = erlmachine_assembly:serial_no(Assembly),
    Module:blocked(SN, Assembly, Part, Extension, Failure).

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

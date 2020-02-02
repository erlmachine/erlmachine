 -module(erlmachine_system).
-behaviour(gen_server).

%% API.
-export([start_link/0]).

-export([schema/1, forms/1]).
-export([schema/2, schema_by_serial_no/2]).
-export([form/1, form/2, form_by_serial_no/2]).
-export([submit/2, submit/3, submit_by_serial_no/3]).

%% gen_server.
-export([
         init/1,
         handle_call/3, handle_cast/2, handle_info/2, 
         terminate/2,
         code_change/3
        ]).

-export([failure/1, failure/2, failure/3]).
-export([success/0, success/1, success/2]).

-export([overtime/3]).

-include("erlmachine_factory.hrl").

-type failure(E, R) :: {error, {E, R}}.
-type failure(E) :: {error, E}.
-type failure(E, R, State) :: {error, {E, R}, State}.
-type success(Result) :: {ok, Result}.
-type success(Result, State) :: {ok, Result, State}.
-type success() :: ok.

-export_type([failure/1, failure/2, failure/3, success/0, success/1, success/2]).

-spec schema_by_serial_no(GearBox::assembly(), SN::serial_no()) ->
                                 success(term()) | failure(term(), term(), term()).
schema_by_serial_no(GearBox, SN) ->
    Assembly = erlmachine_gearbox:find(GearBox, SN),
    schema(GearBox, Assembly).

-spec schema(GearBox::assembly()) ->
                    success(term()) | failure(term(), term(), term()).
schema(GearBox) ->
    SN = erlmachine_assembly:serial_no(GearBox),
    ProtName = erlmachine_assembly:prototype_name(GearBox),

    ProtName:schema(SN, GearBox).

-spec schema(GearBox::assembly(), Assembly::assembly()) ->
                    success(term()) | failure(term(), term(), term()).
schema(GearBox, Assembly) ->
    SN = erlmachine_assembly:serial_no(Assembly),
    ProtName = erlmachine_assembly:prototype_name(Assembly),

    ProtName:schema(SN, GearBox, Assembly).

-spec forms(GearBox::assembly()) ->
                   success(term()) | failure(term(), term(), term()).
forms(GearBox) ->
    Find = erlmachine_gearbox:find(GearBox),
    {ok, Main} = form(GearBox),
    Ext = [begin {ok, Form} = form(GearBox, Part), Form end || Part <- Find],
    {ok, lists:flatten([Main|Ext])}.

-spec form_by_serial_no(GearBox::assembly(), SN::serial_no()) ->
                               success(term()) | failure(term(), term(), term()).
form_by_serial_no(GearBox, SN) ->
    Assembly = erlmachine_gearbox:find(GearBox, SN),
    form(GearBox, Assembly).

-spec form(GearBox::assembly()) ->
                  success(term()) | failure(term(), term(), term()).
form(GearBox) ->
    SN = erlmachine_assembly:serial_no(GearBox),
    ProtName = erlmachine_assembly:prototype_name(GearBox),

    ProtName:form(SN, GearBox).

-spec form(GearBox::assembly(), Assembly::assembly()) ->
                  success(term()) | failure(term(), term(), term()).
form(GearBox, Assembly) ->
    SN = erlmachine_assembly:serial_no(Assembly),
    ProtName = erlmachine_assembly:prototype_name(Assembly),

    ProtName:form(SN, GearBox, Assembly).

-spec submit_by_serial_no(GearBox::assembly(), SN::serial_no(), Form::term()) ->
                                 success(term()) | failure(term(), term(), term()).
submit_by_serial_no(GearBox, SN, Form) ->
    Assembly = erlmachine_gearbox:find(GearBox, SN),
    submit(GearBox, Assembly, Form).

-spec submit(GearBox::assembly(), Form::term()) ->
                    success(term()) | failure(term(), term(), term()).
submit(GearBox, Form) ->
    SN = erlmachine_assembly:serial_no(GearBox),
    ProtName = erlmachine_assembly:prototype_name(GearBox),

    ProtName:submit(SN, GearBox, Form).

-spec submit(GearBox::assembly(), Assembly::assembly(), Form::term()) ->
                    success(term()) | failure(term(), term(), term()).
submit(GearBox, Assembly, Form) ->
    SN = erlmachine_assembly:serial_no(Assembly),
    ProtName = erlmachine_assembly:prototype_name(Assembly),

    ProtName:submit(SN, GearBox, Assembly, Form).

-spec failure(E::term(), R::term()) -> 
                     failure(E::term(), R::term()).
failure(E, R) -> 
    {error, {E, R}}.

-spec failure(E::term()) -> failure(E::term()).
failure(E) ->
    {error, E}.

-spec failure(E::term(), R::term(), S::term()) -> failure(E::term(), R::term(), S::term()).
failure(E, R, S) -> 
    {error, {E, R}, S}.

-spec success(Result::term()) -> success(Result::term()).
success(Result) ->
    {ok, Result}.

-spec success(Result::term(), S::term()) -> success(Result::term(), S::term()).
success(Result, S) -> 
    {ok, Result, S}.

-spec success() -> success().
success() ->
    ok.

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

-spec overtime(Assembly::assembly(), Station::station(), Throughput::integer()) -> 
                      ok.
overtime(_Assembly, _Station, _Throughput) ->
    ok.
 
%% crash, damage, block, overtime, production_limit callbacks will be provided; 

-module(erlmachine_assembly).
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

%% The main purpose of this module is to instantiate proceses accordingly to design file;
%% In this module will be provided incapsulation around building of independent parts and whole transmission as well;

-type serial_number()::erlmachine_serial_number::serial_number().

-type gear()::erlmachine_gear::gear().
-type shaft()::erlmachine_shaft::shaft().
-type axle()::erlmachine_axle::axle().
-type gearbox()::erlmachine_gearbox::gearbox().

-type datasheet()::erlmachine_datasheet::datasheet().

-export_type model_no()::term().
-export_type part_no()::term().

%% Abbreviations M/N and P/N will be represented on name;
-record(model, {
                id::atom(),
                model_no::model_no(),
                product::gear()|shaft()|axle()|gearbox(),
                options::term(),
                part_no::part_no()
               }
       ).

-record(prototype, {
                    id::atom()
                   }
       ).

-export_type model()::#model{}.
-export_type prototype()::#prototype{}.

-record (assembly, {
                    serial_number::serial_number(), %% We can get build info (ts, etc..) by serial number from db;
                    prototype::prototype(),
                    model::model(), 
                    datasheet::datasheet()
                   }
        ).

-export_type assembly()::#assembly{}.

-export_type acceptance_criteria()::list().
-export_type accept()::true.
-export_type reject()::list().


%% API.

-spec start_link() -> {ok, pid()}.
start_link() ->
	gen_server:start_link(?MODULE, [], []).

%% gen_server.

-record(state, {}).

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

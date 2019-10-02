-module(axle_base_prototype).

-folder(<<"erlmachine/factory/prototypes/axle_base_prototype">>).

-behaviour(erlmachine_assembly).
-behaviour(erlmachine_tracker).
-behaviour(erlmachine_system).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-include("erlmachine_factory.hrl").
-include("erlmachine_system.hrl").

-spec tag(Package::map()) -> Tag::binary().
tag(#{model := Model}) ->
    ID = atom_to_binary(Model, latin1),
    ID.

format_name(SerialNumber) ->
    ID = erlang:binary_to_atom(SerialNumber, latin1),
    ID.

attach(Name::serial_number(), Part::assembly()) ->
    ok.

detach((Name::serial_number(), Part::assembly() ->
    ok.

stop() ->
    ok.

restart() ->
    ok.

-record(overloaded, {load::load()}).

-spec overloaded(Name::serial_number(), Load::term()) -> Load.
overloaded(Name, Load) ->
    erlang:send(format_name(Name), #overloaded{load = Load}).

installed(Part::assembly()) ->
     ok.

uninstalled(Part::assembly(), Reason::term()) ->
    ok.

install(Name, Parts) ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).
%% I guess we need to provide the concept of broken detail monitoring, supervisor component can provide that;
%% Procs will be provided by datasheet;
%% Restart strategy will be provided as count of broken elements per sec;
%% layout, placement, fixing
init([]) ->
	Procs = [],
	{ok, {{one_for_one, 1, 5}, Procs}}.

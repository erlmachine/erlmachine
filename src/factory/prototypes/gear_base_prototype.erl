-module(gear_base_prototype).

-folder(<<"erlmachine/factory/prototypes/gear_base_prototype">>).

-behaviour(erlmachine_assembly).
-bahaviour(erlmachine_tracker).
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

-include("erlmachine_factory.hrl").
-include("erlmachine_system.hrl").

%% API.

%% I guess factory will write to catalogue via catalogue behaviour;
-spec tag(Package::map()) -> Tag::binary().
tag(#{<<"model">> := Model}) ->
    ID = atom_to_binary(Model, latin1),
    ID.

%% Model is an instance. Determined with serial number and built over prototype.

id(SerialNumber) -> %% I guess serial number preparation can be provided on gearbox instantination by assembly;
    ID = erlang:binary_to_atom(SerialNumber, latin1),
    ID.

-spec install(ID::atom(), Assembly::assembly(), Options::list()) -> success(pid()) | ingnore | failure(E).
install(ID, Assembly, Options) -> %% Appropriate options can be provided by shaft or gearbox;
    gen_server:start_link({local, ID}, ?MODULE, Assembly, Options).

%% I think about ability to reflect both kind of switching - manually or automatically;
%% Shift pattern
-spec switch(ID::atom(), Gear::gear() | Shaft::shaft(), Timeout::timeout()) -> success(Assembly::assembly()) | failure(E, R).
switch() ->
    ok.

overload() ->
    ok.

blockage() ->
    ok.

replace() ->
    ok.

rotate(ID, Motion) ->
    erlang:send(ID, Force),
    Force.

transmit(ID, Motion) ->
    gen_server:call(ID, Force).

-spec uninstall(ID::atom(), Reason::term(), Timeout::timeout()) -> ok.
uninstall(ID, Reason, Timeout) ->
    gen_server:stop(ID, Reason, Timeout).

accept() -> %% TODO Acceptance criteria needs to be satisfied;
     ok.

%% gen_server.

%% From erlmachine_assembly we wait the specialized callback to erlmachine_gear with install/2
init(Assembly::term()) ->
    process_flag(trap_exit, true),
    Package = package(Assembly),
    TrackingNumber = erlmachine_traker:tracking_number(?MODULE, Package),
    erlmachine_traker:trace(TrackingNumber, #{install => Package}),
    {ok, Assembly}.

handle_call(_Request, _From, State) ->
	{reply, ignored, State}.

handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info(_Info, State) ->
	{noreply, State}.

%% When reason is different from normal, or stop - the broken part event is occured;
terminate(_Reason, Assembly) ->
    Package = package(Assembly),
    TrackingNumber = erlmachine_traker:tracking_number(?MODULE, Package),
    erlmachine_traker:trace(TrackingNumber, #{uninstall => Package}),
    ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

package(Assembly) ->
    Model = erlmachine_assembly:model(Assembly),
    SerialNumber = erlmachine_assembly:serial_number(Assembly),
    Package = #{prototype => ?MODULE, model => Model, serial_number => SerialNumber},
    Package.

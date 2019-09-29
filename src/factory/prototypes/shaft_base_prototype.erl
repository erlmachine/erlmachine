-module(shaft_base_prototype).
-behaviour(gen_server).

-folder(<<"erlmachine/factory/prototypes/shaft_base_prototype">>).

-behaviour(erlmachine_assembly).
-behaviour(erlmachine_tracker).
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

format_name(SerialNumber) ->
    ID = erlang:binary_to_atom(SerialNumber, latin1),
    ID.

-spec install(Name::serial_number(), Assembly::assembly(), Options::list()) -> success(pid()) | ingnore | failure(E).
install(Name, Assembly, Options) ->
    gen_server:start_link({local, format_name(SN)}, ?MODULE, Assembly, Options).


-record(attach, {assembly::assembly()}).%% Shift pattern

-spec attach(Name::serial_number(), Part::assembly(), Timeout::timeout()) -> success(Assembly::assembly()) | failure(E, R).
attach(Name, Assembly, Timeout) ->
    gen_server:call(name(Name), #attach{assembly = Assembly}, Timeout).

-record(detach, {id::serial_number()}).

-spec detach(Name::serial_number(), ID::serial_number(), Timeout::timeout()) -> success(Assembly::assembly()) | failure(E, R).
detach(Name, ID, TimeOut) ->
    gen_server:call(name(Name), #detach{id = ID}, TimeOut).

-record(overload, {load::load()}).

-spec overload(Name::serial_number(), Load::term()) -> Load::term().
overload(Name, Load) ->
    erlang:send(format_name(Name), #load{load = Load}).

-record(blockage, {part::assembly()}).

-spec blockage(Name::serial_number(), Part::assembly()) -> Part::assembly().
blockage() ->
    erlang:send(Name, #blockage{part = Part}).

-spec replace(Name::serial_number(), Repair::assembly()) -> success(Assembly::assembly()) | failure(E, R).
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
    %% I guess tracking time will be filled by tracker itself;
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

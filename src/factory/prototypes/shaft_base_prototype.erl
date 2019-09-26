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

format(SerialNumber) ->
    ID = erlang:binary_to_atom(SerialNumber, latin1),
    ID.

-spec install(ID::serial_number(), Assembly::assembly(), Options::list()) -> success(pid()) | ingnore | failure(E).
install(SN, Assembly, Options) ->
    gen_server:start_link({local, format(ID)}, ?MODULE, Assembly, Options).

%% Shift pattern
-spec attach(Name::serial_number(), Assembly::assembly(), Timeout::timeout()) -> success(Assembly::assembly()) | failure(E, R).
attach() ->
    ok.

-spec detach(Name::serial_number(), ID::serial_number(), Timeout::timeout()) -> success(Assembly::assembly()) | failure(E, R).
detach() ->
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

-spec uninstall(ID::atom(), Reason::term(), Timeout::timeout()) -> ok.
uninstall(ID, Reason, Timeout) ->
    gen_server:stop(ID, Reason, Timeout).

accept() -> %% TODO Acceptance criteria needs to be satisfied;
     ok.

%% gen_server.

%% From erlmachine_assembly we wait the specialized callback to erlmachine_gear with install/2
init(Assembly::term()) ->
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

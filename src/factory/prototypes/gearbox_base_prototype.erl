-module(gearbox_base_prototype).

-folder(<<"erlmachine/factory/prototypes/gearbox_base_prototype">>).

-behaviour(erlmachine_assembly).
-behaviour(erlmachine_transmission).
-behaviour(erlmachine_tracker).
-behaviour(erlmachine_system).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-include("erlmachine_factory.hrl").
-include("erlmachine_system.hrl").

%% Gearbox seems look like catalogue from my point of perspective;
%% It is a place where statistics is collected;
%% It can be reflected like view around topology;

format_name(SerialNumber) ->
    ID = erlang:binary_to_atom(SerialNumber, latin1),
    ID.

-spec tag(Package::map()) -> Tag::binary().
tag(#{model := Model}) ->
    ID = atom_to_binary(Model, latin1),
    ID.

-spec overloaded(Name::serial_number(),  Assembly::assembly(), Load::term()) -> Load.
overloaded(Name, Assembly, Load) ->
    %% I guess tracking time will be filled by tracker itself;
    Package = package(Assembly),
    TrackingNumber = erlmachine_traker:tracking_number(?MODULE, Package),
    erlmachine_traker:trace(TrackingNumber, #{overloaded => Package, load => Load}), 
    erlmachine_gearbox:overloaded(Assembly, Load).

-spec blocked(Name::serial_number(), Assembly::assembly(), Part::assembly(), Damage::failure(E, R)) -> ok.
blocked(Name, Assembly, Part, Damage) ->
    Package = package(Assembly),
    TrackingNumber = erlmachine_traker:tracking_number(?MODULE, Package),
    erlmachine_traker:trace(TrackingNumber, #{blocked => Package, part => Part, damage => Damage}),
    %% erlmachine_system:damage(Assembly, Damage), That will be produced by system itself;
    erlmachine_gearbox:blocked(Assembly, Part, Damage).

-spec attached(Name::serial_number(), Assembly::assembly(), Part::assembly()) -> ok.
attached(Name, Assembly, Part) ->
    SerialNumber = erlmachine_factory:serial_number(Part),
    Package = package(Assembly),
    TrackingNumber = erlmachine_traker:tracking_number(?MODULE, Package),
    erlmachine_traker:trace(TrackingNumber, #{attach => Package, part => SerialNumber}),
    erlmachine_gearbox:attached(Assembly, Part).

-spec detached(Name::serial_number(), Assembly::assembly(), ID::serial_number()) -> ok.
detached(Name, Assembly, ID) ->
    %% (Reason == normal) orelse erlmachine_system:crash(Reason, Assembly),
    Package = package(Assembly),
    TrackingNumber = erlmachine_traker:tracking_number(?MODULE, Package),
    erlmachine_traker:trace(TrackingNumber, #{detach => Package, part => ID}),
    erlmachine_gearbox:detached(Assembly, Part).

-spec replaced(Name::serial_number(), Assembly::assembly(), Repair::assembly() -> ok.
replaced(Name, Assembly, Repair) ->
    SerialNumber = erlmachine_factory:serial_number(Repair),
    Package = package(Assembly),
    TrackingNumber = erlmachine_traker:tracking_number(?MODULE, Package),
    erlmachine_traker:trace(TrackingNumber, #{replace => Package, repair => SerialNumber}),
    erlmachine_gearbox:replaced(Assembly, Repair).

-spec switched(Name::serial_number(), Part::assembly(), Timeout::timeout()) -> success(Release::assembly()) | failure(E, R).
switched(Name, Part, Timeout) ->
    SerialNumber = erlmachine_factory:serial_number(Repair),
    Package = package(Assembly),
    TrackingNumber = erlmachine_traker:tracking_number(?MODULE, Package),
    erlmachine_traker:trace(TrackingNumber, #{replace => Package, repair => SerialNumber}),
    erlmachine_gearbox:replaced(Assembly, Repair).

-spec accepted(Name::serial_number(), Criteria::acceptance_criteria()) -> accept() | reject().
accepted(Name, Criteria) -> %% I plan to reflect criteria in datasheet; 
    SerialNumber = erlmachine_factory:serial_number(Repair),
    Package = package(Assembly),
    erlmachine_traker:trace(TrackingNumber, #{accept => Package}).

-spec rejected(Name::serial_number(), Criteria::acceptance_criteria()) -> accept() | reject().
rejected(Name, Criteria) -> %% I plan to reflect criteria in datasheet; 
    SerialNumber = erlmachine_factory:serial_number(Repair),
    Package = package(Assembly),
    erlmachine_traker:trace(TrackingNumber, #{reject => Package, report => Result}),
    erlmachine_system:reject(Reason, Assembly).

attach() -> %% Attach and detach are an optional callbacks, cause is different strategies;
     ok.

detach() ->
    ok.

install(Name, Gearbox) ->
    supervisor:start_link({local, format_name(Name)}, ?MODULE, Gearbox).

init(Gearbox) ->
    SerialNumber = erlmachine_factory:serial_number(Repair),
    Package = package(Assembly),
    erlmachine_traker:trace(TrackingNumber, #{reject => Package, report => Result}),
    Procs = [],
    {ok, {{one_for_one, 1, 5}, Procs}}.

package(Assembly) ->
    Model = erlmachine_assembly:model(Assembly),
    SerialNumber = erlmachine_assembly:serial_number(Assembly),
    Package = #{prototype => ?MODULE, model => Model, serial_number => SerialNumber},
    Package.

trace(Package) ->
    TrackingNumber = erlmachine_traker:tracking_number(?MODULE, Package),
    erlmachine_traker:trace(TrackingNumber, Package).

%% This callback is optional;
uninstall(Name, Gearbox, Reason) ->
    %% We need to be careful with parts whoose are located outside of supervision;
    %% They don't terminate within supervision tree;
    exit(whereis(format_name(Name)), Reason),
    SerialNumber = erlmachine_factory:serial_number(Repair),
    Package = package(Assembly),
    erlmachine_traker:trace(TrackingNumber, #{reject => Package, report => Result}).




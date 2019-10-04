-module(gearbox_base_prototype).
-behaviour(supervisor).

-folder(<<"erlmachine/factory/prototypes/gearbox_base_prototype">>).

-behaviour(erlmachine_assembly).
-behaviour(erlmachine_transmission).
-behaviour(erlmachine_tracker).
-behaviour(erlmachine_system).

-export([start_link/0]).
-export([init/1]).

-include("erlmachine_factory.hrl").
-include("erlmachine_system.hrl").

-spec tag(Package::map()) -> Tag::binary().
tag(#{model := Model}) ->
    ID = atom_to_binary(Model, latin1),
    ID.

-record(gearbox, {
                  input::assembly(),
                  parts=[]::list(assembly()),
                  placement::term(),
                  %% Placement can be implemented by various ways and then represented by different formats; 
                  %% Each implementation can do that over its own discretion;
                  %% Erlmachine do that accordingly to YAML format;
                  output::assembly()
                 }
       ).

-spec overloaded(Name::serial_number(),  Assembly::assembly(), Load::term()) -> Load.
overloaded(Name, Assembly, Load) ->
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

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
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


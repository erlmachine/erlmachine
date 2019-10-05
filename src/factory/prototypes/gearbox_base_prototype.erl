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

%% Gearbox is designed with idea to be as catalogued storage for any design changes inside of particular instance;
%% It is the place where statistics to collect;
%% It can be reflected like formated and customized view around whole topology;
%% I guess some kind of persistence will be provided;
%% We can write any topology changes to our persistence storage;
%% After that we will be able to build reflection of whole topology which was stored before;

format_name(SerialNumber) ->
    ID = erlang:binary_to_atom(SerialNumber, latin1),
    ID.

-spec tag(Package::map()) -> Tag::binary().
tag(#{model := Model}) ->
    ID = atom_to_binary(Model, latin1),
    ID.

-spec installed(Name::serial_number(), Assembly::assembly(), Part::assembly()) ->
                      ok.
installed(Name, Assembly, Part) ->
    trace(Assembly, #{installed => Part}),
    erlmachine_gearbox:installed(Assembly, Part),
    ok.

-spec overloaded(Name::serial_number(), Assembly::assembly(), Part::assembly(), Load::term()) ->
                        ok.
overloaded(Name, Assembly, Part, Load) ->
    %% I guess tracking time can be filled by tracker itself;
    trace(Assembly, #{overloaded => Part, load => Load}),
    erlmachine_gearbox:overloaded(Assembly, Part, Load),
    ok.

-spec blocked(Name::serial_number(), Assembly::assembly(), Part::assembly(), Failure::failure(E, R)) ->
                     ok.
blocked(Name, Assembly, Part, Failure) ->
    erlmachine_gearbox:blocked(Assembly, Part, Failure),
    trace(Assembly, #{blocked => Part, damage => Failure}),
    ok.

-spec attached(Name::serial_number(), Assembly::assembly(), Part::assembly(), Extension::assembly()) ->
                      ok.
attached(Name, Assembly, Part, Extension) ->
    erlmachine_gearbox:attached(Assembly, Part, Extension),
    trace(Assembly, #{attached => Part, extension => Extension}), 
    ok.

-spec detached(Name::serial_number(), Assembly::assembly(),  Part::assembly(), Extension::assembly()) -> 
                      ok.
detached(Name, Assembly, Part, ID) ->
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

-spec uninstalled(Name::serial_number(), Assembly::assembly(), Part::assembly()) ->
                     ok.
uninstalled(Name, Assembly, Part, Reason) ->
    erlmachine_gearbox:uninstalled(Assembly, Part, Reason),
    trace(Assembly, #{uninstalled => Part, reason => Reason}),
    ok.

attach() -> %% Attach and detach are an optional callbacks, cause is on different strategies;
     ok.

detach() ->
    ok.

-record(install, {assembly::assembly(), parts=list(assembly()), procs=list(map()), options=list(tuple)}).

-spec install(Name::serial_number(), Assembly::assembly(), Parts::list(assembly()), Procs::list(map()), Options::list(tuple())) -> 
                     success(pid()) | ingnore | failure(E).
install(Name, Gearbox, Parts) ->
    supervisor:start_link({local, format_name(Name)}, ?MODULE, Gearbox).

init(#install{assembly=Assembly, parts=Parts, procs=Procs, options=Options}) -> 
    %% Procs need to be prepared by builder before;
    %% Potentially we can validate on procs count at that place and on attach/detach calls too;
    Strategy = one_for_all,
    Intensity = proplists:get_value(intensity, Options, 1),
    Period = proplists:get_value(period, Options, 5),
    Spec = #{strategy => Strategy, intensity => Intensity, period => Period},
    trace(Assembly, #{install => Parts, spec => Spec}),
    erlmachine_gearbox:install(Assembly, Parts),
    {ok, {Spec, Procs}}.

-spec uninstall(Name::serial_number(), Assembly::assembly(), Reason::term()) ->
                       ok.
uninstall(Name, Assembly, Reason) ->
    %% We need to be careful with parts whoose are located outside of supervision;
    %% They don't terminate within supervision tree;
    exit(whereis(format_name(Name)), Reason),
    trace(Assembly, #{install => Parts, spec => Spec}),
    erlmachine_gearbox:uninstall(Assembly, Reason),
    ok.

trace(Assembly, Report) ->
    Model = erlmachine_assembly:model(Assembly),
    SerialNumber = erlmachine_assembly:serial_number(Assembly),
    Package = #{prototype => ?MODULE, model => Model, serial_number => SerialNumber},
    TrackingNumber = erlmachine_traker:tracking_number(?MODULE, Package),
    erlmachine_traker:trace(TrackingNumber, Package#{insight => Insight}).


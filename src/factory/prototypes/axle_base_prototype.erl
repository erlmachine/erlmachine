-module(axle_base_prototype).

-folder(<<"erlmachine/factory/prototypes/axle_base_prototype">>).

%%-behaviour(erlmachine_assembly).
%%-behaviour(erlmachine_transmission).
-behaviour(erlmachine_tracker).
%%-behaviour(erlmachine_system).
-behaviour(supervisor).

-export([
         installed/3, uninstalled/4,
         attach/4, detach/4, install/5, uninstall/3, accept/3
        ]).

-export([tag/1]).

-export([init/1]).

-include("erlmachine_factory.hrl").
-include("erlmachine_system.hrl").

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
    erlmachine_axle:installed(Assembly, Part),
    ok.

-spec uninstalled(Name::serial_number(), Assembly::assembly(), Part::assembly(), Reason::term()) ->
                     ok.
uninstalled(Name, Assembly, Part, Reason) ->
    erlmachine_axle:uninstalled(Assembly, Part, Reason),
    trace(Assembly, #{uninstalled => Part, reason => Reason}),
    ok.

-spec attach(Name::serial_number(), Assembly::assembly(), Part::assembly(), Proc::map()) ->
                    success(Child::child()) | {ok, Child::child(), Info::term()} | failure(E).
attach(Name, Assembly, Part, Proc) ->
    trace(Assembly, #{attach => Part}),
    supervisor:start_child({local, format_name(Name)}, Proc).

-spec detach(Name::serial_number(), Assembly::assembly(), Part::assembly(), ID::serial_number()) -> 
                     success() | failure(E).
detach(Name, Assembly, Part, ID) ->
    SupRef = {local, format_name(Name)},
    trace(Assembly, #{detach => Part}),
    supervisor:terminate_child(SupRef, Id),
    supervisor:delete_child(SupRef, ID).

-record(install, {assembly::assembly(), parts=list(assembly()), procs=list(map()), options=list(tuple)}).

-spec install(Name::serial_number(), Assembly::assembly(), Parts::list(assembly()), Procs::list(map()), Options::list(tuple())) -> 
                     success(pid()) | ingnore | failure(E).
install(Name, Assembly, Parts, Procs, Options) ->
    Args = #install{assembly=Assembly, parts=Parts, procs=Procs, options=Options},
    supervisor:start_link({local, format_name(Name)}, ?MODULE, Args).

init(#install{assembly=Assembly, parts=Parts, procs=Procs, options=Options}) ->
    Strategy = one_for_all,
    Intensity = proplists:get_value(intensity, Options, 1),
    Period = proplists:get_value(period, Options, 5),
    trace(Assembly, #{install => Parts}),
    {ok, _} = erlmachine_axle:install(Assembly, Parts),
    {ok, {#{strategy => Strategy, intensity => Intensity, period => Period}, Procs}}.

-spec uninstall(Name::serial_number(), Assembly::assembly(), Reason::term()) ->
                       ok.
uninstall(Name, Assembly, Reason) ->
    exit(whereis(format_name(Name)), Reason),
    trace(Assembly, #{uninstall => Reason, spec => Spec}),
    {ok, _} = erlmachine_axle:uninstall(Assembly, Reason),
    ok.

-spec accept(Name::serial_number(), Assembly::assembly(), Criteria::acceptance_criteria()) ->
                    accept() | reject().
accept(Name, Assembly, Criteria) ->
    Result = {ok, _} = erlmachine_axle:accept(Assembly, Criteria),
    trace(Assembly, #{accept => Result, criteria => Criteria}),
    Result.

trace(Assembly, Report) ->
    ModelName = erlmachine_assembly:model_name(Assembly),
    SerialNumber = erlmachine_assembly:serial_no(Assembly),
    Package = #{prototype => ?MODULE, model_name => Model, serial_no => SerialNumber},
    TrackingNumber = erlmachine_traker:tracking_no(?MODULE, Package),
    erlmachine_traker:trace(TrackingNumber, Package#{insight => Insight}).

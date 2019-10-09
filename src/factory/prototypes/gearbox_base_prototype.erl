-module(gearbox_base_prototype).

-folder(<<"erlmachine/factory/prototypes/gearbox_base_prototype">>).

-behaviour(erlmachine_tracker).

-behaviour(supervisor).


-export([
         install/4,
         attach/5, detach/5, 
         accept/4,
         uninstall/4
        ]).

-export([
         installed/4,
         attached/4, detached/4,
         replaced/4,
         switched/4,
         overloaded/4, blocked/5,
         uninstalled/5,
         accepted/5 rejected/5
        ]).

-export([init/1]).

-export([tag/1]).

-include("erlmachine_factory.hrl").
-include("erlmachine_system.hrl").

%% Gearbox is designed with idea to act like catalogued storage for any design changes inside of particular instance;
%% It is the place where statistics tend to be collected;
%% It can be reflected like formated and customized view around whole topology;
%% I guess some kind of persistence will be provided;
%% We can write any topology changes to our persistence storage;
%% After that we will be able to build reflection of whole topology which was stored before;

-spec tag(Package::map()) -> Tag::binary().
tag(#{model := Model}) ->
    ID = atom_to_binary(Model, latin1),
    ID.

format_name(SerialNumber) ->
    ID = erlang:binary_to_atom(SerialNumber, latin1),
    ID.

-spec installed(Name::serial_no(), GearBox::assembly(), Part::assembly()) ->
                      ok.
installed(_Name, GearBox, Part) ->
    trace(GearBox, #{installed => Part}),
    ok.

-spec uninstalled(Name::serial_no(), GearBox::assembly(), Part::assembly(), Reason::term()) ->
                         ok.
uninstalled(_Name, GearBox, Part, Reason) ->
    trace(Assembly, #{uninstalled => Part, reason => Reason}),
    ok.

-spec overloaded(Name::serial_number(), GearBox::assembly(), Part::assembly(), Load::term()) ->
                        ok.
overloaded(_Name, GearBox, Part, Load) ->
    %% I guess the tracking time can be filled by tracker itself;
    trace(GearBox, #{overloaded => Part, load => Load}),
    ok.

-spec blocked(Name::serial_no(), GearBox::assembly(), Part::assembly(), Extension::assembly(), Failure::term()) ->
                     ok.
blocked(_Name, GearBox, Part, Extension, Failure) ->
    trace(GearBox, #{blocked => Part, extension => Extension, damage => Failure}),
    ok.

-spec attached(Name::serial_no(), GearBox::assembly(), Part::assembly(), Extension::assembly()) ->
                      ok.
attached(_Name, GearBox, Part, Extension) ->
    trace(GearBox, #{attached => Part, extension => Extension}), 
    ok.

-spec detached(Name::serial_no(), GearBox::assembly(), Part::assembly(), Extension::assembly()) ->
                      ok.
detached(_Name, GearBox, Part, Extension) ->
    trace(GearBox, #{detached => Part, extension => Extension}),
    ok.

-spec replaced(Name::serial_no(), GearBox::assembly(), Part::asssembly(), Extension::assembly()) ->
                      ok.
replaced(_Name, GearBox, Part, Extension) ->
    trace(GearBox, #{replaced => Part, extension => Extension}),
    ok.

-spec switched(Name::serial_no(), GearBox::assembly(), Part::assembly(), Extension::assembly()) ->
                      ok.
switched(_Name, GearBox, Part, Extension) ->
    trace(GearBox, #{switched => Part, extension => Extension}),
    ok.

-spec accepted(Name::serial_no(), GearBox::assembly(), Part::assembly(), Criteria::acceptance_criteria(), Report::term()) -> 
                      ok.
accepted(_Name, GearBox, Part, Criteria, Report) ->
    trace(GearBox, #{accepted => Part, criteria => Criteria, report => Report}),
    ok.

-spec rejected(Name::serial_no(),  GearBox::assembly(), Part::assembly(), Criteria::acceptance_criteria(), Report::term()) -> 
                      ok.
rejected(_Name, GearBox, Part, Criteria, Report) ->
    trace(GearBox, #{rejected => Part, criteria => Criteria, report => Report}),
    ok.

-spec attach(Name::serial_no(), Assembly::assembly(), Part::assembly(), Proc::map()) ->
                    success(Child::child()) | {ok, Child::child(), Info::term()} | failure(E).
attach(Name, Assembly, Part, Proc) ->
    trace(Assembly, #{attach => Part}),
    supervisor:start_child({local, format_name(Name)}, Proc).

-spec detach(Name::serial_no(), Assembly::assembly(), Part::assembly(), ID::serial_number()) -> 
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
    %% Procs need to be prepared by builder before;
    %% Potentially we can validate on procs count at that place and on attach/detach calls too;
    Strategy = one_for_all,
    Intensity = proplists:get_value(intensity, Options, 1),
    Period = proplists:get_value(period, Options, 5),
    trace(Assembly, #{install => Parts}),
    {ok, _} = erlmachine_gearbox:install(Assembly, Parts),
    {ok, {#{strategy => Strategy, intensity => Intensity, period => Period}, Procs}}.

-spec uninstall(Name::serial_number(), Assembly::assembly(), Reason::term()) ->
                       ok.
uninstall(Name, Assembly, Reason) ->
    %% We need to be careful with parts whoose are located outside of supervision;
    %% They don't terminate within supervision tree;
    exit(whereis(format_name(Name)), Reason),
    trace(Assembly, #{uninstall => Reason, spec => Spec}),
    {ok, _} = erlmachine_gearbox:uninstall(Assembly, Reason),
    ok.

-spec accept(Name::serial_number(), Assembly::assembly(), Criteria::acceptance_criteria()) ->
                    accept() | reject().
accept(Name, Assembly, Criteria) ->
    Result = {ok, _} = erlmachine_gearbox:accept(Assembly, Criteria),
    trace(Assembly, #{accept => Result, criteria => Criteria}),
    Result.

trace(Assembly, Report) ->
    ModelName = erlmachine_assembly:model_name(Assembly),
    SerialNumber = erlmachine_assembly:serial_no(Assembly),
    Package = #{prototype => ?MODULE, model_name => Model, serial_no => SerialNumber},
    TrackingNumber = erlmachine_traker:tracking_no(?MODULE, Package),
    erlmachine_traker:trace(TrackingNumber, Package#{insight => Insight}).


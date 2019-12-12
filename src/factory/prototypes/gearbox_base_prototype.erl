-module(gearbox_base_prototype).

-folder(<<"erlmachine/factory/prototypes/gearbox_base_prototype">>).

-behaviour(erlmachine_tracker).

-behaviour(supervisor).

-export([name/0]).

-export([
         install/3,
         attach/4, detach/3, 
         accept/3,
         uninstall/3
        ]).

-export([
         installed/3,
         attached/4, detached/4,
         replaced/4,
         overloaded/4, blocked/5,
         uninstalled/4,
         accepted/5, rejected/5
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


-spec name() -> Name::atom().
name() ->
    ?MODULE.

format_name(SerialNumber) ->
    ID = erlang:binary_to_atom(SerialNumber, latin1),
    ID.

-spec installed(Name::serial_no(), GearBox::assembly(), Part::assembly()) ->
                      ok.
installed(_Name, GearBox, Part) ->
    SN = erlmachine_assembly:serial_no(Part),
    trace(GearBox, #{installed => SN}),
    ok.

-spec uninstalled(Name::serial_no(), GearBox::assembly(), Part::assembly(), Reason::term()) ->
                         ok.
uninstalled(_Name, GearBox, Part, Reason) ->
    SN = erlmachine_assembly:serial_no(Part),
    trace(GearBox, #{uninstalled => SN, reason => Reason}),
    ok.

-spec overloaded(Name::serial_no(), GearBox::assembly(), Part::assembly(), Load::term()) ->
                        ok.
overloaded(_Name, GearBox, Part, Load) ->
    SN = erlmachine_assembly:serial_no(Part),
    %% I guess the tracking time can be filled by tracker itself;
    trace(GearBox, #{overloaded => SN, load => Load}),
    ok.

-spec blocked(Name::serial_no(), GearBox::assembly(), Part::assembly(), Extension::assembly(), Failure::term()) ->
                     ok.
blocked(_Name, GearBox, Part, Extension, Failure) ->
    SN = erlmachine_assembly:serial_no(Part),
    trace(GearBox, #{blocked => SN, extension => Extension, damage => Failure}),
    ok.

-spec attached(Name::serial_no(), GearBox::assembly(), Part::assembly(), Extension::assembly()) ->
                      ok.
attached(_Name, GearBox, Part, Extension) ->
    SN = erlmachine_assembly:serial_no(Part),
    trace(GearBox, #{attached => SN, extension => Extension}), 
    ok.

-spec detached(Name::serial_no(), GearBox::assembly(), Part::assembly(), Extension::assembly()) ->
                      ok.
detached(_Name, GearBox, Part, Extension) ->
    SN = erlmachine_assembly:serial_no(Part),
    trace(GearBox, #{detached => SN, extension => Extension}),
    ok.

-spec replaced(Name::serial_no(), GearBox::assembly(), Part::assembly(), Extension::assembly()) ->
                      ok.
replaced(_Name, GearBox, Part, Extension) ->
    SN = erlmachine_assembly:serial_no(Part),
    trace(GearBox, #{replaced => SN, extension => Extension}),
    ok.

-spec accepted(Name::serial_no(), GearBox::assembly(), Part::assembly(), Criteria::acceptance_criteria(), Report::term()) -> 
                      ok.
accepted(_Name, GearBox, Part, Criteria, Report) ->
    SN = erlmachine_assembly:serial_no(Part),
    trace(GearBox, #{accepted => SN, criteria => Criteria, report => Report}),
    ok.

-spec rejected(Name::serial_no(),  GearBox::assembly(), Part::assembly(), Criteria::acceptance_criteria(), Report::term()) -> 
                      ok.
rejected(_Name, GearBox, Part, Criteria, Report) ->
    SN = erlmachine_assembly:serial_no(Part),
    trace(GearBox, #{rejected => SN, criteria => Criteria, report => Report}),
    ok.

-spec attach(Name::serial_no(), GearBox::assembly(), Register::term(), Extension::assembly()) ->
                    success(Release::assembly()) | failure(E::term(), R::term()).
attach(Name, GearBox, Register, Extension) ->
    Result = {ok, Part, Release} = erlmachine_gearbox:attach(GearBox, Register, Extension),
    %% TODO Conditional case for Result needs to be processed;
    Spec = spec(Release, Part),
    %% Mount time will be determined by prototype;
    SupRef = format_name(Name),
    {ok, _PID} = supervisor:start_child(SupRef, Spec),
    trace(GearBox, #{attach => erlmachine_assembly:serial_no(Part)}),
    Result.
    
-spec detach(Name::serial_no(), GearBox::assembly(), ID::serial_no()) ->
                    success(Child::term()) | success(Child::term(), Info::term()) | failure(E::term()).
detach(Name, GearBox, ID) ->
    trace(GearBox, #{detach => ID}),
    Result = {ok, _} = erlmachine_gearbox:detach(GearBox, ID),
    SupRef = format_name(Name),
    ok = supervisor:terminate_child(SupRef, ID),
    ok = supervisor:delete_child(SupRef, ID), %% ID the same for chield and SN
    Result.

-record(install, {gearbox::assembly(), options::list(tuple)}).

-spec install(Name::serial_no(), GearBox::assembly(), Options::list(tuple())) -> 
                     success(pid()) | ingnore | failure(E::term()).
install(Name, GearBox, Opt) ->
    Args = #install{gearbox=GearBox, options=Opt},
    supervisor:start_link({local, format_name(Name)}, ?MODULE, Args).

init(#install{gearbox=GearBox, options=Opt}) ->
    Strategy = proplists:get_value(strategy, Opt, one_for_one),
    {ok, Release} = erlmachine_gearbox:install(GearBox),
    Specs = specs(Release),
    Intensity = proplists:get_value(intensity, Opt, 1),
    Period = proplists:get_value(period, Opt, 5),
    {ok, {#{strategy => Strategy, intensity => Intensity, period => Period}, Specs}}.

-spec uninstall(Name::serial_no(), GearBox::assembly(), Reason::term()) ->
                       ok.
uninstall(Name, GearBox, Reason) ->
    exit(whereis(format_name(Name)), Reason),
    IDs = [erlmachine_assembly:serial_no(Part) || Part <- erlmachine_gearbox:parts(GearBox)],
    trace(GearBox, #{uninstall => IDs, reason => Reason}),
    {ok, _} = erlmachine_gearbox:uninstall(GearBox, Reason),
    ok.

-spec accept(Name::serial_no(), GearBox::assembly(), Criteria::acceptance_criteria()) ->
                    accept() | reject().
accept(_Name, GearBox, Criteria) ->
    {ok, Report, _Release} = erlmachine_gearbox:accept_model(GearBox, Criteria),
    Report.


-spec spec(GearBox::assembly(), Part::assembly()) -> Spec::map().
spec(GearBox, Part) ->
    SN = erlmachine_assembly:serial_no(Part),
    Module = erlmachine_assembly:prototype_name(Part),
    Opt = erlmachine_assembly:prototype_options(Part),
 
    Start = {Module, install, [SN, erlmachine_assembly:parts(GearBox,[]), Part, Opt]},
 
    Restart = proplists:get_value(restart, Opt, permanent),
    Shutdown = proplists:get_value(shutdown, Opt, 5000),
    Modules = proplists:get_value(modules, Opt, [Module]),

    Type = proplists:get_value(type, erlmachine_assembly:assembly_options(Part)),
    #{id => SN, start => Start, restart => Restart, shutdown => Shutdown, modules => Modules, type => Type}.

-spec specs(GearBox::assembly()) -> list(map()).
specs(GearBox) ->
    Parts = erlmachine_assembly:parts(GearBox),
    Specs = [spec(GearBox, Part)|| Part <- Parts],
    Specs.

trace(Assembly, Insight) ->
    Model = erlmachine_assembly:model_name(Assembly),
    SN = erlmachine_assembly:serial_no(Assembly),
    Package = #{prototype => ?MODULE, model => Model, serial_no => SN},
    TN = erlmachine_tracker:tracking_no(?MODULE, Package),
    erlmachine_tracker:trace(TN, Package#{insight => Insight}).

-module(gearbox_base_prototype).

-folder(<<"erlmachine/factory/prototypes/gearbox_base_prototype">>).

-behaviour(erlmachine_tracker).

-behaviour(supervisor).

-export([name/0]).

-export([
         install/3,
         attach/4, detach/4, 
         accept/3,
         uninstall/3
        ]).

-export([
         installed/3,
         attached/4, detached/4,
         replaced/4,
         switched/4,
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
tag(#{model_name := Model}) ->
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
    trace(GearBox, #{uninstalled => Part, reason => Reason}),
    ok.

-spec overloaded(Name::serial_no(), GearBox::assembly(), Part::assembly(), Load::term()) ->
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

-spec replaced(Name::serial_no(), GearBox::assembly(), Part::assembly(), Extension::assembly()) ->
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

-spec attach(Name::serial_no(), GearBox::assembly(), Part::assembly(), Spec::map()) ->
                    success(Child::term()) | success(Child::term(), Info::term()) | failure(E::term()).
attach(Name, GearBox, Part, Spec) ->
    trace(GearBox, #{attach => Part}),
    Res = supervisor:start_child({local, format_name(Name)}, Spec),
    {ok, _} = erlmachine_gearbox:attach(GearBox, Part),
    Res.
    
-spec detach(Name::serial_no(), GearBox::assembly(), ID::serial_no(), ChieldID::term()) ->
                    success(Child::term()) | success(Child::term(), Info::term()) | failure(E::term()).
detach(Name, GearBox, ID, ChieldID) ->
    SupRef = {local, format_name(Name)},
    trace(GearBox, #{detach => ID}),
    supervisor:terminate_child(SupRef, ChieldID),
    Res = supervisor:delete_child(SupRef, ChieldID), %% ID the same for chield and SN
    erlmachine_axle:detach(GearBox, ID),
    Res.

-record(install, {gearbox::assembly(), options::list(tuple)}).

-spec install(Name::serial_no(), GearBox::assembly(), Options::list(tuple())) -> 
                     success(pid()) | ingnore | failure(E::term()).
install(Name, GearBox, Options) ->
    Args = #install{gearbox=GearBox, options=Options},
    supervisor:start_link({local, format_name(Name)}, ?MODULE, Args).

init(#install{gearbox=GearBox, options=Options}) ->
    Strategy = one_for_one,
    {ok, Release} = erlmachine_gearbox:install(GearBox),
    Specs = erlmachine_gearbox:specs(Release),
    Intensity = proplists:get_value(intensity, Options, 1),
    Period = proplists:get_value(period, Options, 5),
    {ok, {#{strategy => Strategy, intensity => Intensity, period => Period}, Specs}}.

-spec uninstall(Name::serial_no(), GearBox::assembly(), Reason::term()) ->
                       ok.
uninstall(Name, GearBox, Reason) ->
    exit(whereis(format_name(Name)), Reason),
    Parts = erlmachine_gearbox:parts(GearBox),
    trace(GearBox, #{uninstall => Parts, reason => Reason}),
    {ok, _} = erlmachine_gearbox:uninstall_model(GearBox, Reason),
    ok.

-spec accept(Name::serial_no(), GearBox::assembly(), Criteria::acceptance_criteria()) ->
                    accept() | reject().
accept(_Name, GearBox, Criteria) ->
    {ok, Report, _Release} = erlmachine_gearbox:accept_model(GearBox, Criteria),
    Report.

trace(Assembly, Insight) ->
    Model = erlmachine_assembly:model_name(Assembly),
    SerialNumber = erlmachine_assembly:serial_no(Assembly),
    Package = #{prototype => ?MODULE, model_name => Model, serial_no => SerialNumber},
    TrackingNumber = erlmachine_tracker:tracking_no(?MODULE, Package),
    erlmachine_tracker:trace(TrackingNumber, Package#{insight => Insight}).

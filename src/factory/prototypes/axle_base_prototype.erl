-module(axle_base_prototype).

-folder(<<"erlmachine/factory/prototypes/axle_base_prototype">>).
%% We need to provide the specialized prototype behaviour;
-behaviour(erlmachine_tracker).

-behaviour(supervisor).

-export([name/0]).

-export([
         install/4,
         mount/4, unmount/4, 
         accept/4,
         uninstall/4
        ]).

-export([installed/3, uninstalled/4]).

-export([init/1]).

-export([tag/1]).

-include("erlmachine_factory.hrl").
-include("erlmachine_system.hrl").

-spec name() -> Name::atom().
name() ->
    ?MODULE.

format_name(SerialNumber) ->
    ID = erlang:binary_to_atom(SerialNumber, latin1),
    ID.

-spec tag(Package::map()) -> Tag::binary().
tag(#{model_name := Model}) ->
    ID = atom_to_binary(Model, latin1),
    ID.

-spec installed(Name::serial_no(), Axle::assembly(), Part::assembly()) ->
                      ok.
installed(_Name, Axle, Part) ->
    SN = erlmachine_assembly:serial_no(Part),
    trace(Axle, #{installed => SN}),
    ok.

-spec uninstalled(Name::serial_no(), Axle::assembly(), Part::assembly(), Reason::term()) -> 
                         ok.
uninstalled(_Name, Axle, Part, Reason) ->
    trace(Axle, #{uninstalled => Part, reason => Reason}),
    ok.

-spec mount(Name::serial_no(), GearBox::assembly(), Axle::assembly(), Part::assembly()) ->
                    success(Release::assembly()) | failure(E::term(), R::term()).
mount(Name, GearBox, Axle, Part) ->
    trace(Axle, #{mount => Part}),
    Result = {ok, Release} = erlmachine_axle:mount(GearBox, Axle, Part),
    %% TODO Conditional case for Result needs to be processed;
    Spec = erlmachine_axle:spec(GearBox, Release, Part),
    %% Mount time will be determined by prototype;
    R = supervisor:start_child({local, format_name(Name)}, Spec),
    io:format("~nChield start: ~p~n",[R]),
    Result.
    
-spec unmount(Name::serial_no(), GearBox::assembly(), Axle::assembly(), ID::serial_no()) ->
                    success(Child::term()) | success(Child::term(), Info::term()) | failure(E::term()).
unmount(_Name, _GearBox, Axle, ID) ->
    %SupRef = {local, format_name(Name)},
    trace(Axle, #{unmount => ID}),
    %%supervisor:terminate_child(SupRef, ChieldID),
    %%Res = supervisor:delete_child(SupRef, ChieldID), %% ID the same for chield and SN
    %%erlmachine_gearbox:unmount(GearBox, ID),
    {ok, []}.

-record(install, {gearbox::assembly(), axle::assembly(), options::list(tuple)}).

-spec install(Name::serial_no(), GearBox::assembly(), Axle::assembly(), Options::list(tuple())) -> 
                     success(pid()) | ingnore | failure(E::term()).
install(Name, GearBox, Axle, Options) ->
    Args = #install{gearbox=GearBox, axle=Axle, options=Options},
    supervisor:start_link({local, format_name(Name)}, ?MODULE, Args).

init(#install{gearbox=GearBox, axle=Axle, options=Options}) ->
    Strategy = one_for_all,
    {ok, Release} = erlmachine_axle:install(GearBox, Axle),
    Specs = erlmachine_axle:specs(GearBox, Release),
    Intensity = proplists:get_value(intensity, Options, 1),
    Period = proplists:get_value(period, Options, 5),
    {ok, {#{strategy => Strategy, intensity => Intensity, period => Period}, Specs}}.

%% I guess later we need some way to adress axle instance inside gearbox;
%% Cause persistence only gearbox and the direction can look like gerabox -> SN -> SN (axle);
%% This approach can also be used to walk through topology;

-spec uninstall(Name::serial_no(), GearBox::assembly(), Axle::assembly(), Reason::term()) ->
                       ok.
uninstall(Name, GearBox, Axle, Reason) ->
    exit(whereis(format_name(Name)), Reason),
    Parts = erlmachine_axle:parts(Axle),
    trace(Axle, #{uninstall => Parts, reason => Reason}),
    {ok, _} = erlmachine_axle:uninstall_model(GearBox, Axle, Reason),
    ok.

-spec accept(Name::serial_no(), GearBox::assembly(), Axle::assembly(), Criteria::acceptance_criteria()) ->
                    accept() | reject().
accept(_Name, GearBox, Axle, Criteria) ->
    {ok, Report, _Release} = erlmachine_axle:accept_model(GearBox, Axle, Criteria),
    Report.

trace(Assembly, Insight) ->
    Model = erlmachine_assembly:model_name(Assembly),
    SN = erlmachine_assembly:serial_no(Assembly),
    Package = #{prototype => ?MODULE, model_name => Model, serial_no => SN},
    TN = erlmachine_tracker:tracking_no(?MODULE, Package),
    erlmachine_tracker:trace(TN, Package#{insight => Insight}).

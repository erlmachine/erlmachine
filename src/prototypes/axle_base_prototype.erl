-module(axle_base_prototype).

-folder(<<"erlmachine/factory/prototypes/axle_base_prototype">>).
%% We need to provide the specialized prototype behaviour;
-behaviour(erlmachine_tracker).

-behaviour(supervisor).

-export([name/0]).

-export([
         install/4,
         attach/5, detach/4, 
         accept/4,
         uninstall/4
        ]).

-export([installed/4, uninstalled/5]).

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
tag(#{model := Model}) ->
    ID = atom_to_binary(Model, latin1),
    ID.

-spec installed(Name::serial_no(), GearBox::assembly(), Axle::assembly(), Part::assembly()) ->
                      ok.
installed(_Name, _GearBox, Axle, Part) ->
    SN = erlmachine_assembly:serial_no(Part),
    trace(Axle, #{installed => SN}),
    ok.

-spec uninstalled(Name::serial_no(), GearBox::assembly(), Axle::assembly(), Part::assembly(), Reason::term()) -> 
                         ok.
uninstalled(_Name, _GearBox, Axle, Part, Reason) ->
    SN = erlmachine_assembly:serial_no(Part),
    trace(Axle, #{uninstalled => SN, reason => Reason}),
    ok.

-spec attach(Name::serial_no(), GearBox::assembly(), Axle::assembly(), Register::term(), Extension::assembly()) ->
                    success(Release::assembly()) | failure(E::term(), R::term()).
attach(Name, GearBox, Axle, Register, Extension) ->
    Result = {ok, Part, Release} = erlmachine_axle:attach(GearBox, Axle, Register, Extension),

    %% TODO Conditional case for Result needs to be processed;
    Spec = spec(GearBox, Release, Part),
    %% Mount time will be determined by prototype;
    SupRef = format_name(Name),

    {ok, _PID} = supervisor:start_child(SupRef, Spec),
    Result.
    
-spec detach(Name::serial_no(), GearBox::assembly(), Axle::assembly(), ID::serial_no()) ->
                    success(Child::term()) | success(Child::term(), Info::term()) | failure(E::term()).
detach(Name, GearBox, Axle, ID) ->
    SupRef = format_name(Name),

    Result = {ok, _} = erlmachine_axle:detach(GearBox, Axle, ID),

    ok = supervisor:terminate_child(SupRef, ID),
    ok = supervisor:delete_child(SupRef, ID), %% ID the same for chield and SN
    Result.

-record(install, {gearbox::assembly(), axle::assembly(), options::list(tuple)}).

-spec install(Name::serial_no(), GearBox::assembly(), Axle::assembly(), Options::list(tuple())) -> 
                     success(pid()) | ingnore | failure(E::term()).
install(Name, GearBox, Axle, Opt) ->
    ID = {local, format_name(Name)},
    Command = #install{ gearbox=GearBox, axle=Axle, options=Opt },

    supervisor:start_link(ID, ?MODULE, Command).

init(#install{gearbox=GearBox, axle=Axle, options=Opt}) ->
    Strategy = proplists:get_value(strategy, Opt, one_for_all),

    {ok, Release} = erlmachine_axle:install(GearBox, Axle),

    Specs = specs(GearBox, Release),
    Intensity = proplists:get_value(intensity, Opt, 1),
    Period = proplists:get_value(period, Opt, 5),

    {ok, {#{strategy => Strategy, intensity => Intensity, period => Period}, Specs}}.

%% I guess later we need some way to adress axle instance inside gearbox;
%% Cause persistence only gearbox and the direction can look like gerabox -> SN -> SN (axle);
%% This approach can also be used to walk through topology;

-spec uninstall(Name::serial_no(), GearBox::assembly(), Axle::assembly(), Reason::term()) ->
                       success().
uninstall(Name, GearBox, Axle, Reason) ->
    exit(whereis(format_name(Name)), Reason),
    erlmachine_axle:uninstall(GearBox, Axle, Reason).

-spec accept(Name::serial_no(), GearBox::assembly(), Axle::assembly(), Criteria::criteria()) ->
                    success() | failure(E::term(), R::term(), S::term()).
accept(_Name, GearBox, Axle, Criteria) ->
    {ok, Status, _} = erlmachine_axle:accept(GearBox, Axle, Criteria),
    Status.

%% TODO
%% I am going to provide mnesia gears, mongodb , etc..
%% Process manager will be responsible for persistance storage management

-spec spec(GearBox::assembly(), Axle::assembly(), Part::assembly()) -> Spec::map().
spec(GearBox, _Axle, Part) ->
    SN = erlmachine_assembly:serial_no(Part),
    Module = erlmachine_assembly:prototype_name(Part),
    Opt = erlmachine_assembly:prototype_options(Part),
  
    Start = {Module, install, [SN, erlmachine_assembly:parts(GearBox,[]), Part, Opt]},
 
    Restart = proplists:get_value(restart, Opt, permanent),
    Shutdown = proplists:get_value(shutdown, Opt, 5000),
    Modules = proplists:get_value(modules, Opt, [Module]),

    Type = proplists:get_value(type, erlmachine_assembly:assembly_options(Part)),
    #{id => SN, start => Start, restart => Restart, shutdown => Shutdown, modules => Modules, type => Type}.

-spec specs(GearBox::assembly(), Axle::assembly()) -> list(map()).
specs(GearBox, Axle) ->
    Parts = erlmachine_assembly:parts(Axle),
    Specs = [spec(GearBox, Axle, Part)|| Part <- Parts],
    Specs.

trace(Assembly, Insight) ->
    Model = erlmachine_assembly:model_name(Assembly),
    SN = erlmachine_assembly:serial_no(Assembly),
    Package = #{prototype => ?MODULE, model => Model, serial_no => SN},
    TN = erlmachine_tracker:tracking_no(?MODULE, Package),
    erlmachine_tracker:trace(TN, Package#{insight => Insight}).

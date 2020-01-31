-module(axle_base_prototype).

-folder(<<"erlmachine/prototypes/axle_base_prototype">>).
%% We need to provide the specialized prototype behaviour;
-behaviour(erlmachine_tracker).

-behaviour(supervisor).

-export([name/0]).

%% supervisor.
-export([init/1]).

-export([
         install/4,
         attach/5, detach/4, 
         accept/4,
         uninstall/4
        ]).

-export([installed/4, uninstalled/5]).

-export([schema/3]).

-export([tag/1]).

-include("erlmachine_factory.hrl").
-include("erlmachine_system.hrl").

-spec name() -> Name::atom().
name() ->
    ?MODULE.

format_name(SerialNumber) ->
    ID = erlang:binary_to_atom(SerialNumber, latin1),
    ID.

-spec tag(Axle::assembly()) -> Tag::binary().
tag(Axle) ->
    Model = erlmachine_assembly:model_name(Axle),
    ID = atom_to_binary(Model, latin1),
    ID.

-spec installed(Name::serial_no(), GearBox::assembly(), Axle::assembly(), Part::assembly()) ->
                      success().
installed(_Name, _GearBox, Axle, Part) ->
    SN = erlmachine_assembly:serial_no(Axle),
    to_track(SN, #{ installed => erlmachine_assembly:serial_no(Part) }),
    ok.

-spec uninstalled(Name::serial_no(), GearBox::assembly(), Axle::assembly(), Part::assembly(), Reason::term()) -> 
                         success().
uninstalled(_Name, _GearBox, Axle, Part, Reason) ->
    SN = erlmachine_assembly:serial_no(Axle),
    to_track(SN, #{uninstalled => erlmachine_assembly:serial_no(Part), reason => Reason}),
    ok.

-spec attach(Name::serial_no(), GearBox::assembly(), Axle::assembly(), Register::term(), Extension::assembly()) ->
                    success(assembly()) | failure(term(), term()).
attach(Name, GearBox, Axle, Register, Extension) ->
    {ok, Part, Release} = erlmachine_axle:attach(GearBox, Axle, Register, Extension),

    %% TODO Conditional case for Result needs to be processed;
    Spec = spec(GearBox, Release, Part),
    %% Mount time will be determined by prototype;
    SupRef = format_name(Name),

    {ok, _PID} = supervisor:start_child(SupRef, Spec),

    SN = erlmachine_assembly:serial_no(Axle),
    to_track(SN, #{ attach => erlmachine_assembly:serial_no(Extension) }),
    erlmachine:success(Part, Release).
    
-spec detach(Name::serial_no(), GearBox::assembly(), Axle::assembly(), ID::serial_no()) ->
                    success(term()) | success(term(), term()) | failure(term()).
detach(Name, GearBox, Axle, ID) ->
    SupRef = format_name(Name),

    {ok, Rel} = erlmachine_axle:detach(GearBox, Axle, ID),

    ok = supervisor:terminate_child(SupRef, ID),
    ok = supervisor:delete_child(SupRef, ID), %% ID the same for chield and SN

    SN = erlmachine_assembly:serial_no(Axle),
    to_track(SN, #{ detach => ID }),
    erlmachine:success(Rel).

-record(install, {gearbox::assembly(), axle::assembly(), options::list(tuple)}).

-spec install(Name::serial_no(), GearBox::assembly(), Axle::assembly(), Options::list(tuple())) -> 
                     success(pid()) | ingnore | failure(E::term()).
install(Name, GearBox, Axle, Opt) ->
    SN = erlmachine_assembly:serial_no(Axle),

    ID = {local, format_name(Name)},
    Command = #install{ gearbox=GearBox, axle=Axle, options=Opt },

    Res = supervisor:start_link(ID, ?MODULE, Command),
 
    to_track(SN, #{ install => ts() }),
    Res.

init(#install{gearbox=GearBox, axle=Axle, options=Opt}) ->
    Strategy = proplists:get_value(strategy, Opt, one_for_all),

    {ok, Release} = erlmachine_axle:install(GearBox, Axle),

    Specs = specs(GearBox, Release),
    Int = proplists:get_value(intensity, Opt, 1),
    Per = proplists:get_value(period, Opt, 5),

    erlmachine:success({#{strategy => Strategy, intensity => Int, period => Per}, Specs}).

%% I guess later we need some way to adress axle instance inside gearbox;
%% Cause persistence only gearbox and the direction can look like gerabox -> SN -> SN (axle);
%% This approach can also be used to walk through topology;

-spec uninstall(Name::serial_no(), GearBox::assembly(), Axle::assembly(), Reason::term()) ->
                       success().
uninstall(Name, GearBox, Axle, Reason) ->
    exit(whereis(format_name(Name)), Reason),

    {ok, Rel} = erlmachine_axle:uninstall(GearBox, Axle, Reason),

    SN = erlmachine_assembly:serial_no(Axle),
    to_track(SN, #{uninstall => ts()}),
    erlmachine:success(Rel).

-spec accept(Name::serial_no(), GearBox::assembly(), Axle::assembly(), Criteria::criteria()) ->
                    success(term()) | failure(E::term(), R::term(), S::term()).
accept(_Name, GearBox, Axle, Criteria) ->
    {ok, Res, Rel} = erlmachine_axle:accept(GearBox, Axle, Criteria),

    SN = erlmachine_assembly:serial_no(Axle),
    to_track(SN, #{ accept => Res }),
    erlmachine:success(Res, Rel).

-spec schema(Name::serial_no(), GearBox::assembly(), Axle::assembly()) ->
                    success(term()) | failure(term(), term()).
schema(_Name, GearBox, Axle) ->
    {ok, Schema, Rel} = erlmachine_axle:schema(GearBox, Axle),
    erlmachine:success(Schema, Rel).

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

    Type = proplists:get_value(type, Opt),
    #{id => SN, start => Start, restart => Restart, shutdown => Shutdown, modules => Modules, type => Type}.

-spec specs(GearBox::assembly(), Axle::assembly()) -> list(map()).
specs(GearBox, Axle) ->
    Parts = erlmachine_assembly:parts(Axle),
    Specs = [spec(GearBox, Axle, Part)|| Part <- Parts],
    Specs.

-spec to_track(TN::serial_no(), Package::map()) -> success().
to_track(TN, Package) ->
    erlmachine_tracker:track(TN, Package), 
    ok.

-spec ts() -> integer().
ts() ->
    erlmachine:timestamp().

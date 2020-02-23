-module(gearbox_base_prototype).

-folder(<<"erlmachine/prototypes/gearbox_base_prototype">>).

-behaviour(erlmachine_tracker).

-behaviour(supervisor).

-export([name/0]).

%% supervisor.
-export([init/1]).

%% erlmachine_assembly
-export([install/3, uninstall/3, attach/4, detach/3]).
-export([installed/3, uninstalled/4, attached/4, detached/4]).

%% erlmachine_factory
-export([accept/3]).
-export([accepted/4, rejected/4]).

%% erlmachine_system
-export([form/2, submit/3]).
-export([blocked/5, overloaded/4]).

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

-spec installed(Name::serial_no(), GearBox::assembly(), Part::assembly()) ->
                      success().
installed(_Name, GearBox, Part) ->
    erlmachine_gearbox:installed(GearBox, Part).

-spec uninstalled(Name::serial_no(), GearBox::assembly(), Part::assembly(), Reason::term()) ->
                         success().
uninstalled(_Name, GearBox, Part, Reason) ->
    erlmachine_gearbox:uninstalled(GearBox, Part, Reason).

-spec overloaded(Name::serial_no(), GearBox::assembly(), Part::assembly(), Load::term()) ->
                        success().
overloaded(_Name, GearBox, Part, Load) ->
    erlmachine_gearbox:overloaded(GearBox, Part, Load).

-spec blocked(Name::serial_no(), GearBox::assembly(), Part::assembly(), E::term(), R::term()) ->
                     success().
blocked(_Name, GearBox, Part, E, R) ->
    erlmachine_gearbox:blocked(GearBox, Part, E, R).

-spec attached(Name::serial_no(), GearBox::assembly(), Part::assembly(), Ext::assembly()) ->
                      success().
attached(_Name, GearBox, Part, Ext) ->
    erlmachine_gearbox:attached(GearBox, Part, Ext).

-spec detached(Name::serial_no(), GearBox::assembly(), Part::assembly(), Ext::assembly()) ->
                      success().
detached(_Name, GearBox, Part, Ext) ->
    erlmachine_gearbox:detached(GearBox, Part, Ext).

-spec accepted(Name::serial_no(), GearBox::assembly(), Ext::assembly(), Criteria::criteria()) -> 
                      success().
accepted(_Name, GearBox, Ext, Criteria) ->
    erlmachine_gearbox:accepted(GearBox, Ext, Criteria).

-spec rejected(Name::serial_no(), GearBox::assembly(), Ext::assembly(), Criteria::criteria()) -> 
                      success().
rejected(_Name, GearBox, Ext, Criteria) ->
    erlmachine_gearbox:rejected(GearBox, Ext, Criteria).

-spec name() -> Name::atom().
name() ->
    ?MODULE.

format_name(SerialNumber) ->
    ID = erlang:binary_to_atom(SerialNumber, latin1),
    ID.

-record(install, {gearbox::assembly(), options::list(tuple)}).

-spec install(Name::serial_no(), GearBox::assembly(), Opt::list(tuple())) -> 
                     success(pid()) | ingnore | failure(term()).
install(Name, GearBox, Opt) ->
    Command = #install{ gearbox=GearBox, options=Opt },
    supervisor:start_link({local, format_name(Name)}, ?MODULE, Command).

init(#install{ gearbox=GearBox, options=Opt }) ->
    Strategy = proplists:get_value(strategy, Opt, one_for_one),

    {ok, Rel} = erlmachine_gearbox:install(GearBox),

    Specs = specs(Rel),
    Int = proplists:get_value(intensity, Opt, 1),
    Per = proplists:get_value(period, Opt, 5),
    erlmachine:success({#{strategy => Strategy, intensity => Int, period => Per}, Specs}).

-spec attach(Name::serial_no(), GearBox::assembly(), Reg::term(), Ext::assembly()) ->
                    success(assembly()) | failure(term(), term()).
attach(Name, GearBox, Reg, Ext) ->
    {ok, Part, Rel} = erlmachine_gearbox:attach(GearBox, Reg, Ext),

    %% TODO Conditional case for Result needs to be processed;
    Spec = spec(Rel, Part),
    %% Mount time will be determined by prototype;
    SupRef = format_name(Name),

    {ok, _PID} = supervisor:start_child(SupRef, Spec),
    erlmachine:success(Part, Rel).

-spec detach(Name::serial_no(), GearBox::assembly(), ID::serial_no()) ->
                    success() | success(term(), term()) | failure(term()).
detach(Name, GearBox, ID) ->
    {ok, Rel} = erlmachine_gearbox:detach(GearBox, ID),

    SupRef = format_name(Name),
    ok = supervisor:terminate_child(SupRef, ID),
    ok = supervisor:delete_child(SupRef, ID), %% ID the same for chield and SN
    erlmachine:success(Rel).

-spec uninstall(Name::serial_no(), GearBox::assembly(), Reason::term()) ->
                       success().
uninstall(Name, GearBox, Reason) ->
    exit(whereis(format_name(Name)), Reason),

    {ok, _} = erlmachine_gearbox:uninstall(GearBox, Reason),
    erlmachine:success().

-spec accept(Name::serial_no(), GearBox::assembly(), Criteria::acceptance_criteria()) ->
                    success() | failure(term(), term(), term()).
accept(_Name, GearBox, Criteria) ->
    {ok, Res, _} = erlmachine_gearbox:accept(GearBox, Criteria),
    erlmachine:success(Res).

-spec form(Name::serial_no(), GearBox::assembly()) -> 
                  success(term()) | failure(term(), term(), term()).
form(_Name, GearBox) ->
    {ok, Form, _} = erlmachine_gearbox:form(GearBox),
    erlmachine:success(Form).

-spec submit(Name::serial_no(), GearBox::assembly(), Form::term()) -> 
                    success(term()) | failure(term(), term(), term()).
submit(_Name, GearBox, Form) ->
    {ok, Res, _} = erlmachine_gearbox:submit(GearBox, Form),
    erlmachine:success(Res).

-spec spec(GearBox::assembly(), Part::assembly()) -> map().
spec(GearBox, Part) ->
    SN = erlmachine_assembly:serial_no(Part),
    Module = erlmachine_assembly:prototype_name(Part),
    Opt = erlmachine_assembly:prototype_options(Part),

    Start = {Module, install, [SN, erlmachine_assembly:parts(GearBox,[]), Part, Opt]},

    Restart = proplists:get_value(restart, Opt, permanent),
    Shutdown = proplists:get_value(shutdown, Opt, 5000),
    Modules = proplists:get_value(modules, Opt, [Module]),

    Type = proplists:get_value(type, Opt),
    Label = erlmachine_assembly:label(Part),
    #{
      id => Label,
      start => Start,
      restart => Restart, 
      shutdown => Shutdown,
      modules => Modules, 
      type => Type
     }.

-spec specs(GearBox::assembly()) -> list(map()).
specs(GearBox) ->
    Parts = erlmachine_assembly:parts(GearBox),
    Specs = [spec(GearBox, Part)|| Part <- Parts],
    Specs.

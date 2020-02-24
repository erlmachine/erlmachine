-module(axle_base_prototype).

-behaviour(erlmachine_tracker).

-behaviour(supervisor).

-export([name/0]).

%% supervisor.
-export([init/1]).

%% erlmachine_assembly
-export([install/4, uninstall/4, attach/5, detach/4]).

%% erlmachine_factory
-export([accept/4]).

%% erlmachine_system
-export([form/3, submit/4]).

-export([tag/1]).

-include("erlmachine_factory.hrl").
-include("erlmachine_system.hrl").

-spec name() -> Name::atom().
name() ->
    ?MODULE.

-spec format_name(SN::serial_no()) -> 
                         atom().
format_name(SN) ->
    erlang:binary_to_atom(SN, latin1).

-spec tag(Axle::assembly()) -> Tag::binary().
tag(Axle) ->
    Model = erlmachine_assembly:model_name(Axle),
    atom_to_binary(Model, latin1).

-record(install, { gearbox::assembly(), axle::assembly(), options::list() }).

-spec install(Name::serial_no(), GearBox::assembly(), Axle::assembly(), Opt::list()) -> 
                     success(pid()) | ingnore | failure(term()).
install(Name, GearBox, Axle, Opt) ->
    Com = #install{ gearbox=GearBox, axle=Axle, options=Opt },

    supervisor:start_link({local, format_name(Name)}, ?MODULE, Com).

init(#install{ gearbox=GearBox, axle=Axle, options=Opt }) ->
    Strategy = proplists:get_value(strategy, Opt, one_for_all),
    Int = proplists:get_value(intensity, Opt, 1),
    Per = proplists:get_value(period, Opt, 5),

    {ok, Rel} = erlmachine_axle:install(GearBox, Axle),
    erlmachine:success({#{strategy => Strategy, intensity => Int, period => Per}, specs(GearBox, Rel)}).

%% I guess later we need some way to adress axle instance inside gearbox;
%% Cause persistence only gearbox and the direction can look like gerabox -> SN -> SN (axle);
%% This approach can also be used to walk through topology;

-spec attach(Name::serial_no(), GearBox::assembly(), Axle::assembly(), Reg::term(), Ext::assembly()) ->
                    success(assembly(), assembly()) | failure(term(), term()).
attach(Name, GearBox, Axle, Reg, Ext) ->
    {ok, Part, Rel} = erlmachine_axle:attach(GearBox, Axle, Reg, Ext),
    %% TODO Conditional case for Result needs to be processed;
    %% Mount time will be determined by prototype;
    {ok, _PID} = supervisor:start_child(format_name(Name), spec(GearBox, Rel, Part)),
    erlmachine:success(Part, Rel).

-spec detach(Name::serial_no(), GearBox::assembly(), Axle::assembly(), Id::term()) ->
                    success() | success(term(), term()) | failure(term()).
detach(Name, GearBox, Axle, Id) ->
    {ok, Rel} = erlmachine_axle:detach(GearBox, Axle, Id),

    SupRef = format_name(Name),
    ok = supervisor:terminate_child(SupRef, Id),
    ok = supervisor:delete_child(SupRef, Id),
    erlmachine:success(Rel).

-spec uninstall(Name::serial_no(), GearBox::assembly(), Axle::assembly(), Reason::term()) ->
                       success(assembly()).
uninstall(Name, GearBox, Axle, Reason) ->
    exit(whereis(format_name(Name)), Reason),

    {ok, Rel} = erlmachine_axle:uninstall(GearBox, Axle, Reason),
    erlmachine:success(Rel).

-spec accept(Name::serial_no(), GearBox::assembly(), Axle::assembly(), Criteria::criteria()) ->
                    success(term()) | failure(term(), term(), term()).
accept(_Name, GearBox, Axle, Criteria) ->
    {ok, Res, _} = erlmachine_axle:accept(GearBox, Axle, Criteria),

    erlmachine:success(Res).

-spec form(Name::serial_no(), GearBox::assembly(), Axle::assembly()) ->
                  success(term()) | failure(term(), term(), term()).
form(_Name, GearBox, Axle) ->
    {ok, Form, _} = erlmachine_axle:form(GearBox, Axle),
    erlmachine:success(Form).

-spec submit(Name::serial_no(), GearBox::assembly(), Axle::assembly(), Form::term()) -> 
                    success(term()) | failure(term(), term(), term()).
submit(_Name, GearBox, Axle, Form) ->
    {ok, Res, _} = erlmachine_axle:submit(GearBox, Axle, Form),
    erlmachine:success(Res).

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
    Label = erlmachine_assembly:label(Part),
    #{
      id => Label, 
      start => Start, 
      restart => Restart, 
      shutdown => Shutdown, 
      modules => Modules,
      type => Type
     }.

-spec specs(GearBox::assembly(), Axle::assembly()) -> list(map()).
specs(GearBox, Axle) ->
    Parts = erlmachine_assembly:parts(Axle),
    Specs = [spec(GearBox, Axle, Part)|| Part <- Parts],
    Specs.

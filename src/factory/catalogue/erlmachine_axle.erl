-module(erlmachine_axle).

-export([
         install/2,
         attach/4, detach/3,
         accept/3,
         uninstall/3
        ]).

-export([form/2, submit/3]).

-export([axle/0]).

-export([parts/2]).

-export([record_name/0, attributes/0]).

-include("erlmachine_factory.hrl").
-include("erlmachine_system.hrl").

-callback install(SN::serial_no(), IDs::list(serial_no()), Schema::term(), Opt::term(), Env::list()) -> 
    success(term()) | failure(term(), term(), term()) | failure(term()).

-callback uninstall(SN::serial_no(), Reason::term(), Schema::term()) -> 
    success(term()) | failure(term(), term(), term()) | failure(term()).

-callback accept(SN::serial_no(), Criteria::criteria(), Schema::term()) -> 
    success(term(), term()) | failure(term(), term(), term()).

-callback attach(SN::serial_no(), Register::term(), ID::serial_no(), Schema::term()) -> 
    success(term()) | failure(term(), term(), term()) | failure(term()).

-callback detach(SN::serial_no(), ID::serial_no(), Schema::term()) -> 
    success(term()) | failure(term(), term(), term()) | failure(term()).

-callback form(SN::serial_no(), State::term()) ->
    success(term(), term()) | failure(term(), term(), term()) | failure(term()).

-callback submit(SN::serial_no(), Form::term(), State::term()) ->
    success(term()) | failure(term(), term(), term()) | failure(term()).

-optional_callbacks([form/2, submit/3]).

-record(axle, { }).

%% I am thinking about graph ipmlementation of body;
-type axle() :: #axle{}. %% Here is place for describing product attributes;

-export_type([axle/0]).

-spec record_name() -> atom().
record_name() ->
    axle.

-spec attributes() -> list(atom()).
attributes() ->
    record_info(fields, axle).

-spec axle() -> axle().
axle() ->
    #axle{ }.

-spec install(GearBox::assembly(), Axle::assembly()) -> 
                     success(assembly()) | failure(term(), term(), term()).
install(GearBox, Axle) ->
    ModelName = erlmachine_assembly:model_name(Axle),
    SN = erlmachine_assembly:serial_no(Axle),
    Env = erlmachine_gearbox:env(GearBox), 
    Options = erlmachine_assembly:model_options(Axle),
    IDs = [erlmachine_assembly:serial_no(Part)|| Part <- erlmachine_assembly:parts(Axle)], 

    {ok, State} = ModelName:install(SN, IDs, state(Axle), Options, Env),
    
    %% We are going to add error handling later; 
    Release = state(Axle, State),
    erlmachine_assembly:installed(GearBox, Release),
    {ok, Release}.

-spec attach(GearBox::assembly(), Axle::assembly(), Register::term(), Extension::assembly()) ->
                    success(assembly(), assembly()) | failure(term(), term(), term()).
attach(GearBox, Axle, Register, Extension) ->
    ModelName = erlmachine_assembly:model_name(Axle),
    SN = erlmachine_assembly:serial_no(Axle), ID = erlmachine_assembly:serial_no(Extension),
    
    {ok, State} = ModelName:attach(SN, Register, ID, state(Axle)),
    
    Part = erlmachine_assembly:mounted(Extension, Axle),
    Release = erlmachine_assembly:add(state(Axle, State), Part),
    erlmachine_assembly:attached(GearBox, Release, Part),
    {ok, Part, Release}. %% TODO

-spec detach(GearBox::assembly(), Axle::assembly(), ID::serial_no()) ->
                    success(assembly()) | failure(term(), term(), term()).
detach(GearBox, Axle, ID) ->
    ModelName = erlmachine_assembly:model_name(Axle),
    SN = erlmachine_assembly:serial_no(Axle),

    {ok, State} = ModelName:detach(SN, ID, state(Axle)),
    
    Release = erlmachine_assembly:remove(state(Axle, State), ID),
    erlmachine_assembly:detached(GearBox, Release, ID),
    {ok, Release}. %% TODO

-spec accept(GearBox::assembly(), Axle::assembly(), Criteria::criteria()) ->
                    success(term(), term()) | failure(term(), term(), term()).
accept(GearBox, Axle, Criteria) ->
    ModelName = erlmachine_assembly:model_name(Axle),
    SN = erlmachine_assembly:serial_no(Axle),

    {ok, Result, State} = ModelName:accept(SN, Criteria, state(Axle)),

    Release = state(Axle, State),
    case Result of 
        ok ->
            erlmachine_factory:accepted(GearBox, Release, Criteria);
        _ ->
            erlmachine_factory:rejected(GearBox, Release, Criteria, Result)
    end,
    {ok, Result, Release}.

-spec uninstall(GearBox::assembly(), Axle::assembly(), Reason::term()) -> 
                       ok.
uninstall(GearBox, Axle, Reason) ->
    ModelName = erlmachine_assembly:model_name(Axle),
    SN = erlmachine_assembly:serial_no(Axle),
    
    {ok, State} = ModelName:uninstall(SN, Reason, state(Axle)),
    
    Release = state(Axle, State),
    erlmachine_assembly:uninstalled(GearBox, Release, Reason),
    ok.

-spec form(GearBox::assembly(), Axle::assembly()) ->
                  success(term(), assembly()) | failure(term(), term(), term()).
form(_GearBox, Axle) ->
    ModelName = erlmachine_assembly:model_name(Axle),
    SN = erlmachine_assembly:serial_no(Axle),

    Mod = ModelName, Fun = form, Args = [SN, state(Axle)],
    Def = erlmachine:success([], state(Axle)),

    {ok, Form, State} = erlmachine:optional_callback(Mod, Fun, Args, Def),
    {ok, Form, state(Axle, State)}.

-spec submit(GearBox::assembly(), Axle::assembly(), Form::term()) ->
                    success(term(), assembly()) | failure(term(), term(), term()).
submit(_GearBox, Axle, Form) ->
    ModelName = erlmachine_assembly:model_name(Axle),
    SN = erlmachine_assembly:serial_no(Axle),

    {ok, Res, State} = ModelName:submit(SN, Form, state(Axle)),
    {ok, Res, state(Axle, State)}.

-spec state(Axle::assembly()) -> term().
state(Axle) ->
    erlmachine_assembly:schema(Axle).

-spec state(Axle::assembly(), Schema::term()) -> assembly().
state(Axle, Schema) ->
    erlmachine_assembly:schema(Axle, Schema).

-spec parts(Axle::assembly(), Parts::list(assembly())) -> Release::assembly().
parts(Axle, Parts) ->
    Mounted = [erlmachine_assembly:mounted(Part, Axle)|| Part <- Parts],
    Release = erlmachine_assembly:parts(Axle, Mounted),
    Release.

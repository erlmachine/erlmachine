-module(erlmachine_axle).

%% erlmachine_assembly
-export([install/2, uninstall/3, attach/4, detach/3]).

%% erlmachine_factory
-export([accept/3]).

%% erlmachine_system
-export([form/2, submit/3]).

-export([axle/0]).

-export([parts/2]).

-export([record_name/0, attributes/0]).

-include("erlmachine_factory.hrl").
-include("erlmachine_system.hrl").

%% erlmachine_assembly callbacks:
-callback install(Label::term(), Ids::list(term()), Schema::term(), Opt::term(), Env::list()) -> 
    success(term()) | failure(term(), term(), term()).

-callback uninstall(Label::term(), Reason::term(), Schema::term()) -> 
    success(term()) | failure(term(), term(), term()).

-callback attach(Label::term(), Reg::term(), Id::term(), Schema::term()) -> 
    success(term()) | failure(term(), term(), term()).

-callback detach(Label::term(), Id::term(), Schema::term()) -> 
    success(term()) | failure(term(), term(), term()).

%% erlmachine_factory callbacks:
-callback accept(Label::term(), Criteria::criteria(), Schema::term()) -> 
    success(term()) | failure(term(), term(), term()).

%% erlmachine_system callbacks:
-callback form(Label::term(), Schema::term()) ->
    success(term(), term()) | failure(term(), term(), term()).

-callback submit(Label::term(), Form::term(), Schema::term()) ->
    success(term(), term()) | failure(term(), term(), term()).

-optional_callbacks([attach/4, detach/3]).
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
    Label = erlmachine_assembly:label(Axle),
    Env = erlmachine_gearbox:env(GearBox), 
    Opt = erlmachine_assembly:model_options(Axle),
    Ids = [erlmachine_assembly:label(Part)|| Part <- erlmachine_assembly:parts(Axle)], 

    case ModelName:install(Label, Ids, state(Axle), Opt, Env) of 
        {ok, State} ->
            Rel = state(Axle, State),
            erlmachine_assembly:installed(GearBox, Rel),
            {ok, Rel};
        {error, E, R, State} ->
            {error, E, R, state(Axle, State)}
    end.

-spec attach(GearBox::assembly(), Axle::assembly(), Reg::term(), Ext::assembly()) ->
                    success(assembly(), assembly()) | failure(term(), term(), term()).
attach(GearBox, Axle, Reg, Ext) ->
    ModelName = erlmachine_assembly:model_name(Axle),
    Label = erlmachine_assembly:label(Axle), Id = erlmachine_assembly:label(Ext),

    case ModelName:attach(Label, Reg, Id, state(Axle)) of 
        {ok, State} ->
            Part = erlmachine_assembly:mounted(Ext, Axle),
            Rel = erlmachine_assembly:add(state(Axle, State), Part),
            erlmachine_assembly:attached(GearBox, Rel, Part),
            {ok, Part, Rel};
        {error, E, R, State} ->
            {error, E, R, state(Axle, State)}
    end.

-spec detach(GearBox::assembly(), Axle::assembly(), Id::serial_no()) ->
                    success(assembly()) | failure(term(), term(), term()).
detach(GearBox, Axle, Id) ->
    ModelName = erlmachine_assembly:model_name(Axle),
    Label = erlmachine_assembly:label(Axle),

    case ModelName:detach(Label, Id, state(Axle)) of
        {ok, State} ->
            Rel = erlmachine_assembly:remove(state(Axle, State), Id),
            erlmachine_assembly:detached(GearBox, Rel, Id),
            {ok, Rel};
        {error, E, R, State} ->
            {error, E, R, state(Axle, State)}
    end.

-spec accept(GearBox::assembly(), Axle::assembly(), Criteria::criteria()) ->
                    success(term(), term()) | failure(term(), term(), term()).
accept(GearBox, Axle, Criteria) ->
    ModelName = erlmachine_assembly:model_name(Axle),
    Label = erlmachine_assembly:label(Axle),

    case ModelName:accept(Label, Criteria, state(Axle)) of 
        {ok, State} ->
            Rel = state(Axle, State),
            erlmachine_factory:accepted(GearBox, Rel, Criteria),
            {ok, Rel};
        {error, E, R, State} ->
            Rel = state(Axle, State),
            erlmachine_factory:rejected(GearBox, Rel, Criteria),
            {error, E, R, Rel}
    end.

-spec uninstall(GearBox::assembly(), Axle::assembly(), Reason::term()) -> 
                       ok.
uninstall(GearBox, Axle, Reason) ->
    ModelName = erlmachine_assembly:model_name(Axle),
    Label = erlmachine_assembly:label(Axle),

    {ok, State} = ModelName:uninstall(Label, Reason, state(Axle)),
    Rel = state(Axle, State),
    erlmachine_assembly:uninstalled(GearBox, Rel, Reason),
    {ok, Rel}.

-spec form(GearBox::assembly(), Axle::assembly()) ->
                  success(term(), assembly()) | failure(term(), term(), term()).
form(_GearBox, Axle) ->
    ModelName = erlmachine_assembly:model_name(Axle),
    Label = erlmachine_assembly:label(Axle),

    Mod = ModelName, Fun = form, Args = [Label, state(Axle)],
    Def = erlmachine:success([], state(Axle)),

    case erlmachine:optional_callback(Mod, Fun, Args, Def) of
        {ok, Form, State} ->
            {ok, Form, state(Axle, State)};
        {error, E, R, State} ->
            {error, E, R, state(Axle, State)} 
    end.

-spec submit(GearBox::assembly(), Axle::assembly(), Form::term()) ->
                    success(term(), assembly()) | failure(term(), term(), term()).
submit(_GearBox, Axle, Form) ->
    ModelName = erlmachine_assembly:model_name(Axle),
    Label = erlmachine_assembly:label(Axle),

    case ModelName:submit(Label, Form, state(Axle)) of
        {ok, Res, State} ->
            {ok, Res, state(Axle, State)};
        {error, E, R, State} ->
            {error, E, R, state(Axle, State)} 
    end.

-spec state(Axle::assembly()) -> term().
state(Axle) ->
    erlmachine_assembly:schema(Axle).

-spec state(Axle::assembly(), Schema::term()) -> assembly().
state(Axle, Schema) ->
    erlmachine_assembly:schema(Axle, Schema).

-spec parts(Axle::assembly(), Parts::list(assembly())) -> assembly().
parts(Axle, Parts) ->
    Mounted = [erlmachine_assembly:mounted(Part, Axle)|| Part <- Parts],
    Rel = erlmachine_assembly:parts(Axle, Mounted),
    Rel.

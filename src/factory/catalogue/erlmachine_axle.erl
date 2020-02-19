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

-callback install(Label::term(), Ids::list(term()), Schema::term(), Opt::term(), Env::list()) -> 
    success(term()) | failure(term(), term(), term()) | failure(term()).

-callback uninstall(Label::term(), Reason::term(), Schema::term()) -> 
    success(term()) | failure(term(), term(), term()) | failure(term()).

-callback accept(Label::term(), Criteria::criteria(), Schema::term()) -> 
    success(term(), term()) | failure(term(), term(), term()).

-callback attach(Label::term(), Reg::term(), Id::term(), Schema::term()) -> 
    success(term()) | failure(term(), term(), term()) | failure(term()).

-callback detach(Label::term(), Id::term(), Schema::term()) -> 
    success(term()) | failure(term(), term(), term()) | failure(term()).

-callback form(Label::term(), Schema::term()) ->
    success(term(), term()) | failure(term(), term(), term()) | failure(term()).

-callback submit(Label::term(), Form::term(), Schema::term()) ->
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
    Label = erlmachine_assembly:label(Axle),
    Env = erlmachine_gearbox:env(GearBox), 
    Opt = erlmachine_assembly:model_options(Axle),
    Ids = [erlmachine_assembly:label(Part)|| Part <- erlmachine_assembly:parts(Axle)], 

    {ok, State} = ModelName:install(Label, Ids, state(Axle), Opt, Env),
    
    %% We are going to add error handling later; 
    Rel = state(Axle, State),
    erlmachine_assembly:installed(GearBox, Rel),
    {ok, Rel}.

-spec attach(GearBox::assembly(), Axle::assembly(), Reg::term(), Ext::assembly()) ->
                    success(assembly(), assembly()) | failure(term(), term(), term()).
attach(GearBox, Axle, Reg, Ext) ->
    ModelName = erlmachine_assembly:model_name(Axle),
    Label = erlmachine_assembly:label(Axle), Id = erlmachine_assembly:label(Ext),
    
    {ok, State} = ModelName:attach(Label, Reg, Id, state(Axle)),
    
    Part = erlmachine_assembly:mounted(Ext, Axle),
    Rel = erlmachine_assembly:add(state(Axle, State), Part),
    erlmachine_assembly:attached(GearBox, Rel, Part),
    {ok, Part, Rel}. %% TODO

-spec detach(GearBox::assembly(), Axle::assembly(), Id::serial_no()) ->
                    success(assembly()) | failure(term(), term(), term()).
detach(GearBox, Axle, Id) ->
    ModelName = erlmachine_assembly:model_name(Axle),
    Label = erlmachine_assembly:label(Axle),

    {ok, State} = ModelName:detach(Label, Id, state(Axle)),
    
    Rel = erlmachine_assembly:remove(state(Axle, State), Id),
    erlmachine_assembly:detached(GearBox, Rel, Id),
    {ok, Rel}. %% TODO

-spec accept(GearBox::assembly(), Axle::assembly(), Criteria::criteria()) ->
                    success(term(), term()) | failure(term(), term(), term()).
accept(GearBox, Axle, Criteria) ->
    ModelName = erlmachine_assembly:model_name(Axle),
    Label = erlmachine_assembly:label(Axle),

    {ok, Res, State} = ModelName:accept(Label, Criteria, state(Axle)),

    Rel = state(Axle, State),
    case Res of
        ok ->
            erlmachine_factory:accepted(GearBox, Rel, Criteria);
        _ ->
            erlmachine_factory:rejected(GearBox, Rel, Criteria, Res)
    end,
    {ok, Res, Rel}.

-spec uninstall(GearBox::assembly(), Axle::assembly(), Reason::term()) -> 
                       ok.
uninstall(GearBox, Axle, Reason) ->
    ModelName = erlmachine_assembly:model_name(Axle),
    Label = erlmachine_assembly:label(Axle),
    
    {ok, State} = ModelName:uninstall(Label, Reason, state(Axle)),
    
    Rel = state(Axle, State),
    erlmachine_assembly:uninstalled(GearBox, Rel, Reason),
    ok.

-spec form(GearBox::assembly(), Axle::assembly()) ->
                  success(term(), assembly()) | failure(term(), term(), term()).
form(_GearBox, Axle) ->
    ModelName = erlmachine_assembly:model_name(Axle),
    Label = erlmachine_assembly:label(Axle),

    Mod = ModelName, Fun = form, Args = [Label, state(Axle)],
    Def = erlmachine:success([], state(Axle)),

    {ok, Form, State} = erlmachine:optional_callback(Mod, Fun, Args, Def),
    {ok, Form, state(Axle, State)}.

-spec submit(GearBox::assembly(), Axle::assembly(), Form::term()) ->
                    success(term(), assembly()) | failure(term(), term(), term()).
submit(_GearBox, Axle, Form) ->
    ModelName = erlmachine_assembly:model_name(Axle),
    Label = erlmachine_assembly:label(Axle),

    {ok, Res, State} = ModelName:submit(Label, Form, state(Axle)),
    {ok, Res, state(Axle, State)}.

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

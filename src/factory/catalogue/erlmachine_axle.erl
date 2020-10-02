-module(erlmachine_axle).

-export([axle/2]).

-export([install/1, uninstall/3, attach/4, detach/3]).

-export([accept/3]).

-export([form/2, submit/3]).

-export([type/0]).

-include("erlmachine_factory.hrl").
-include("erlmachine_system.hrl").

%% At that place some decoration featuries can be provided;
-spec axle(Schema::term(), Model::model()) -> 
                  assembly().
axle(Schema, Model, Env) ->
    Assembly = erlmachine_assembly:assembly(?MODULE, Schema, Model),
    erlmachine_assembly:tag(Assembly, type()).

-spec install(Assembly::assembly()) -> 
                     success(pid()) | failure(term(), term()).
install(Assembly) ->
    erlmachine_supervisor_prototype:install(Assembly).

-spec attach(GearBox::assembly(), Axle::assembly(), Reg::term(), Ext::assembly()) ->
                    success(assembly(), term()) | failure(term(), term(), term()).
attach(GearBox, Axle, Reg, Ext) ->
    ModelName = erlmachine_assembly:model_name(Axle),
    Label = erlmachine_assembly:label(Axle), Id = erlmachine_assembly:label(Ext),

    case ModelName:attach(Label, Reg, Id, state(Axle)) of 
        {ok, State} ->
            Mounted = erlmachine_assembly:mounted(Ext, Axle),
            Rel = erlmachine_assembly:extend(state(Axle, State), Mounted),
            erlmachine_assembly:attached(GearBox, Rel, Mounted),
            erlmachine:success(Mounted, Rel);
        {error, E, R, State} ->
            erlmachine:failure(E, R, state(Axle, State))
    end.

-spec detach(GearBox::assembly(), Axle::assembly(), Id::serial_no()) ->
                    success(term()) | failure(term(), term(), term()).
detach(GearBox, Axle, Id) ->
    ModelName = erlmachine_assembly:model_name(Axle),
    Label = erlmachine_assembly:label(Axle),

    case ModelName:detach(Label, Id, state(Axle)) of
        {ok, State} ->
            Rel = erlmachine_assembly:separate(state(Axle, State), Id),
            erlmachine_assembly:detached(GearBox, Rel, Id),
            erlmachine:success(Rel);
        {error, E, R, State} ->
            erlmachine:failure(E, R, state(Axle, State))
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
            erlmachine:success(Rel);
        {error, E, R, State} ->
            Rel = state(Axle, State),
            erlmachine_factory:rejected(GearBox, Rel, Criteria),
            erlmachine:failure(E, R, Rel)
    end.

-spec uninstall(GearBox::assembly(), Axle::assembly(), Reason::term()) -> 
                       success(term()).
uninstall(GearBox, Axle, Reason) ->
    ModelName = erlmachine_assembly:model_name(Axle),
    Label = erlmachine_assembly:label(Axle),

    {ok, State} = ModelName:uninstall(Label, Reason, state(Axle)),
    Rel = state(Axle, State),
    erlmachine_assembly:uninstalled(GearBox, Rel, Reason),
    erlmachine:success(Rel).

-spec form(GearBox::assembly(), Axle::assembly()) ->
                  success(term(), assembly()) | failure(term(), term(), term()).
form(_GearBox, Axle) ->
    ModelName = erlmachine_assembly:model_name(Axle),
    Label = erlmachine_assembly:label(Axle),

    Mod = ModelName, Fun = form, Args = [Label, state(Axle)],
    Def = erlmachine:success([], state(Axle)),

    case erlmachine:optional_callback(Mod, Fun, Args, Def) of
        {ok, Form, State} ->
            erlmachine:success(Form, state(Axle, State));
        {error, E, R, State} ->
            erlmachine:failure(E, R, state(Axle, State))
    end.

-spec submit(GearBox::assembly(), Axle::assembly(), Form::term()) ->
                    success(term(), assembly()) | failure(term(), term(), term()).
submit(_GearBox, Axle, Form) ->
    ModelName = erlmachine_assembly:model_name(Axle),
    Label = erlmachine_assembly:label(Axle),

    case ModelName:submit(Label, Form, state(Axle)) of
        {ok, Res, State} ->
            erlmachine:success(Res, state(Axle, State));
        {error, E, R, State} ->
            erlmachine:failure(E, R, state(Axle, State))
    end.

-spec state(Axle::assembly()) -> term().
state(Axle) ->
    erlmachine_assembly:schema(Axle).

-spec state(Axle::assembly(), Schema::term()) -> assembly().
state(Axle, Schema) ->
    erlmachine_assembly:schema(Axle, Schema).

-spec extensions(Axle::assembly(), Parts::list(assembly())) -> assembly().
extensions(Axle, Parts) ->
    Mounted = [erlmachine_assembly:mounted(Part, Axle)|| Part <- Parts],
    erlmachine_assembly:extensions(Axle, Mounted).

-spec type() -> atom().
type() ->
    'supervisor'.

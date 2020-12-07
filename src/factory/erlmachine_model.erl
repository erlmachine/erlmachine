-module(erlmachine_model).

%% API.

-export([model/0, model/2]).

-export([
         name/1, name/2,
         options/1, options/2
        ]).

-include("erlmachine_system.hrl").

-record(model, {
                name::atom(),
                options::term()
               }
       ).

-type model() :: #model{}.

-export_type([model/0]).

-spec model() -> model().
model() ->
    #model{}.

-spec model(Name::atom(), Opt::list()) ->
                  model().
model(Name, Opt) ->
    Model = model(),
    options(name(Model, Name), Opt).

-spec name(Model::model()) -> atom().
name(Model) ->
    Model#model.name.

-spec name(Model::model(), Name::atom()) -> model().
name(Model, Name) ->
    Model#model{ name=Name }.

-spec options(Model::model()) -> list().
options(Model) ->
    Model#model.options.

-spec options(Model::model(), Opt::list()) -> model().
options(Model, Opt) ->
    Model#model{ options=Opt }.

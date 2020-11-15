-module(erlmachine_model).

%% API.

-export([model/0, model/3]).

-export([
         name/1, name/2,
         options/1, options/2,
         model_no/1, model_no/2,
         prototype/1, prototype/2
        ]).

-include("erlmachine_system.hrl").

-type model_no() :: binary().

-type prototype() :: erlmachine_factory:prototype().

-record(model, {
                %% A modle_no can act as product configurator to generate a master production schedule;
                model_no::model_no(),
                name::atom(),
                prototype::prototype(),
                options::term(),
                digest::binary()
               }
       ).

-type model() :: #model{}.

-export_type([model/0]).

-spec model() -> model().
model() ->
    #model{}.

-spec model(Name::atom(), Opt::list(), Prot::prototype()) ->
                  model().
model(Name, Opt, Prot) ->
    Model = model(),
    Rel = prototype(options(name(Model, Name), Opt), Prot),
    MN = erlmachine:base64url(erlmachine:md5(Rel)),
    model_no(Rel, MN).

-spec prototype(Model::model()) -> prototype().
prototype(Model) ->
    Model#model.prototype.

-spec prototype(Model::model(), Prototype::prototype()) -> model().
prototype(Model, Prototype) ->
    Model#model{ prototype = Prototype }.

-spec model_no(Model::model()) -> model_no().
model_no(Model) ->
    Model#model.model_no.

-spec model_no(Model::model(), MN::model_no()) -> model().
model_no(Model, MN) ->
    Model#model{ model_no=MN }.

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

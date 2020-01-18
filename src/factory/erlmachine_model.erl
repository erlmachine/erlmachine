-module(erlmachine_model).

%% API.

-export([model/0, model/4]).

-export([
         name/1, name/2,
         options/1, options/2,
         model_no/1, model_no/2,
         product/1, product/2,
         prototype/1, prototype/2, 
         digest/1, digest/2
        ]).

-export([attributes/0]).

-include("erlmachine_system.hrl").

-type model_no() :: binary().

-type prototype() :: erlmachine_factory:prototype().

-type gear() :: erlmachine_gear:gear().
-type axle() :: erlmachine_axle:axle().
-type shaft() :: erlmachine_shaft:shaft().
-type gearbox() :: erlmachine_gearbox:gerbox().

-type product() :: gear() | axle() | gearbox() | shaft().

-record(model, {
                %% A modle_no can act as product configurator to generate a master production schedule;
                model_no::model_no(),
                name::atom(),
                product::product(),
                prototype::prototype(),
                options::term(),
                digest::binary()
               }
       ).

-type model() :: #model{}.

-export_type([model/0, product/0]).

-spec model(Name::atom(), Opt::list(), Prot::prototype(), Product::product()) ->
                  model().
model(Name, Opt, Prot, Product) ->
    Model = (model())#model{ name=Name, options=Opt, prototype=Prot, product=Product },

    MN = erlmachine:digest(Model, base64),

    model_no(digest(Model, MN), MN).

-spec attributes() -> list(atom()).
attributes() ->
    record_info(fields, model).

-spec model() -> model().
model() ->
    #model{}.

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

-spec product(Model::model()) -> product().
product(Model) ->
    Model#model.product.

-spec product(Model::model(), Product::product()) -> model().
product(Model, Product) ->
    Model#model{ product=Product }.

-spec digest(Model::model()) -> binary().
digest(Model) ->
    Model#model.digest.

-spec digest(Model::model(), Digest::binary()) -> model().
digest(Model, Digest) ->
    Model#model{ digest=Digest }.

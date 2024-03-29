-module(erlmachine_model).
%% API.
-export([new/0, new/1, new/2, new/3]).

-export([
         module/1, module/2,
         options/1, options/2,
         vsn/1, vsn/2
        ]).

-include_lib("erlbox/include/erlbox.hrl").

-record(model, {
                %% Module name
                module::module(),
                %% Domain level options which are passed as is
                options::map(),
                %% vsn/1 attribute of the module (MD5 checksum if not specified)
                vsn::term()
               }
       ).

-opaque model() :: #model{}.

-export_type([model/0]).

-spec new() -> model().
new() ->
    #model{}.

-spec new(Module::module()) -> model().
new(Module) ->
    new(Module, _Opt = #{}).

-spec new(Module::module(), Opt::map()) -> model().
new(Module, Opt) ->
    Model = new(),
    options(module(Model, Module), Opt).

-spec new(Module::module(), Opt::map(), Vsn::term()) -> model().
new(Module, Opt, Vsn) ->
    Model = new(Module, Opt),
    vsn(Model, Vsn).

-spec module(Model::model()) -> module().
module(Model) ->
    Model#model.module.

-spec module(Model::model(), Module::module()) -> model().
module(Model, Module) ->
    Model#model{ module = Module }.

-spec options(Model::model()) -> map().
options(Model) ->
    Model#model.options.

-spec options(Model::model(), Opt::map()) -> model().
options(Model, Opt) ->
    Model#model{ options = Opt }.

-spec vsn(Model::model()) -> term().
vsn(Model) ->
    Model#model.vsn.

-spec vsn(Model::model(), Vsn::term()) -> model().
vsn(Model, Vsn) ->
    Model#model{ vsn = Vsn }.

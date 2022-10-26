-module(erlmachine_prototype).

%% API.

-export([new/0, new/1, new/2, new/3]).

-export([
         module/1, module/2,
         options/1, options/2,
         vsn/1, vsn/2
        ]).

-include_lib("erlbox/include/erlbox.hrl").

-record(prototype, {
                    %% Module name
                    module::module(),
                    %% Service level options which are passed as is
                    options::map(),
                    %% vsn/1 attribute of the module (MD5 checksum if not specified)
                    vsn::term()
                   }
       ).

-opaque prototype() :: #prototype{}.

-export_type([prototype/0]).

-spec new() -> prototype().
new() ->
    #prototype{}.

-spec new(Module::module()) -> prototype().
new(Module) ->
    new(Module, _Opt = #{}).

-spec new(Module::module(), Opt::map()) -> prototype().
new(Module, Opt) ->
    Prototype = new(),
    options(module(Prototype, Module), Opt).

-spec new(Module::module(), Opt::map(), Vsn::[term()]) -> prototype().
new(Module, Opt, Vsn) ->
    Prototype = new(Module, Opt),
    vsn(Prototype, Vsn).

-spec module(Prot::prototype()) -> module().
module(Prot) ->
    Prot#prototype.module.

-spec module(Prot::prototype(), Module::module()) -> prototype().
module(Prot, Module) ->
    Prot#prototype{ module = Module }.

-spec options(Prot::prototype()) -> map().
options(Prot) ->
    Prot#prototype.options.

-spec options(Prot::prototype(), Opt::map()) -> prototype().
options(Prot, Opt) ->
    Prot#prototype{ options = Opt }.

-spec vsn(Prot::prototype()) -> term().
vsn(Prot) ->
    Prot#prototype.vsn.

-spec vsn(Prot::prototype(), Vsn::term()) -> prototype().
vsn(Prot, Vsn) ->
    Prot#prototype{ vsn = Vsn }.

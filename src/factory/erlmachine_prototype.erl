-module(erlmachine_prototype).

%% API.

-export([prototype/2]).

-export([
         name/1, name/2,
         options/1, options/2,
         vsn/1, vsn/2
        ]).

-include("erlmachine_system.hrl").

-record(prototype, {
                    name::atom(),
                    options::term(),
                    vsn::term()
                   }
       ).

-type prototype() :: #prototype{}.

-export_type([prototype/0]).

-spec prototype() -> prototype().
prototype() ->
    #prototype{}.

-spec prototype(Name::atom(), Opt::list()) -> prototype().
prototype(Name, Opt) ->
    Prototype = prototype(),
    options(name(Prototype, Name), Opt).

-spec name(Prot::prototype()) -> atom().
name(Prot) ->
    Prot#prototype.name.

-spec name(Prot::prototype(), Name::atom()) -> prototype().
name(Prot, Name) ->
    Prot#prototype{ name=Name }.

-spec options(Prot::prototype()) -> list().
options(Prot) ->
    Prot#prototype.options.

-spec options(Prot::prototype(), Opt::list()) -> prototype().
options(Prot, Opt) ->
    Prot#prototype{ options=Opt }.

-spec vsn(Prot::prototype()) -> term().
vsn(Prot) ->
    Prot#prototype.vsn.

-spec vsn(Prot::prototype(), Vsn::term()) -> prototype().
vsn(Prot, Vsn) ->
    Prot#prototype{ vsn = Vsn }.

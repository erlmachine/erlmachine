-module(erlmachine_prototype).

%% API.

-export([prototype/2]).

-export([
         name/1, name/2,
         options/1, options/2
        ]).

-include("erlmachine_system.hrl").

-record(prototype, { name::atom(), options::term() }).

-type prototype() :: #prototype{}.

-export_type([prototype/0]).

-spec prototype() -> prototype().
prototype() ->
    #prototype{}.

-spec prototype(Name::atom(), Opt::list()) -> prototype().
prototype(Name, Opt) ->
    Prototype = prototype(),
    options(name(Prototype, Name), Opt).

-spec name(Prototype::prototype()) -> atom().
name(Prototype) ->
    Prototype#prototype.name.

-spec name(Prototype::prototype(), Name::atom()) -> prototype().
name(Prototype, Name) ->
    Prototype#prototype{ name=Name }.

-spec options(Prototype::prototype()) -> list().
options(Prototype) ->
    Prototype#prototype.options.

-spec options(Prototype::prototype(), Opt::list()) -> prototype().
options(Prototype, Opt) ->
    Prototype#prototype{ options=Opt }.

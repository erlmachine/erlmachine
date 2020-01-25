-module(erlmachine_prototype).

%% API.

-export([prototype/2]).

-export([
         name/1, name/2,
         options/1, options/2
        ]).

-export([record_name/0, attributes/0]).

-include("erlmachine_system.hrl").

-record(prototype, { name::atom(), options::term() }).

-type prototype() :: #prototype{}.

-export_type([prototype/0]).

-spec record_name() -> atom().
record_name() ->
    prototype.

-spec attributes() -> list(atom()).
attributes() ->
    record_info(fields, prototype).

-spec prototype(Name::atom(), Opt::list()) -> prototype().
prototype(Name, Opt) ->
    (prototype())#prototype{ name=Name, options=Opt }.

-spec prototype() -> prototype().
prototype() ->
    #prototype{}.

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

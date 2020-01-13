-module(erlmachine_prototype).

%% API.

-export([prototype/0, prototype/3]).

-export([
         name/1, name/2,
         body/1, body/2,
         options/1, options/2
        ]).

-export([table/0, attributes/0]).

-include("erlmachine_system.hrl").

-record(prototype, {
                    name::atom(),
                    options::term(),
                    body::term()
                   }
       ).

-type prototype() :: #prototype{}.

-export_type([prototype/0]).

-spec prototype(Name::atom(), Opt::list(), Body::term()) -> prototype().
prototype(Name, Opt, Body) ->
    (prototype())#prototype{ name=Name, options=Opt, body=Body }.

-spec table() -> atom().
table() -> 
    prototype.

-spec attributes() -> list(atom()).
attributes() ->
    record_info(fields, prototype).

-spec prototype() -> prototype().
prototype() ->
    #prototype{}.

-spec name(Prototype::prototype()) -> atom().
name(Prototype) ->
    Prototype#prototype.name.

-spec name(Prototype::prototype(), Name::atom()) -> prototype().
name(Prototype, Name) ->
    Prototype#prototype{ name=Name }.

-spec body(Prototype::prototype()) -> term().
body(Prototype) ->
    Prototype#prototype.body.

-spec body(Prototype::prototype(), Body::term()) -> prototype().
body(Prototype, Body) ->
    Prototype#prototype{ body=Body }.

-spec options(Prototype::prototype()) -> list().
options(Prototype) ->
    Prototype#prototype.options.

-spec options(Prototype::prototype(), Opt::list()) -> prototype().
options(Prototype, Opt) ->
    Prototype#prototype{ options=Opt }.

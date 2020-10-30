-module(erlmachine_schema).
%% This module is responsible to setup the schema for the whole transmittion and to manage it;

-export([new/0]).

-export([add_edge/3]).
-export([add_vertex/2, del_vertex/2]).
-export([in_edges/2, out_edges/2]).

-export([del_path/3]).

-include("erlmachine_factory.hrl").
-include("erlmachine_system.hrl").

-type vertex() :: term().
-type edge() :: term().

-type schema() :: term().

%% Current implementation of a schema is digraph. But we are able to change it on demand;
-spec new() -> term().
new() ->
    digraph:new().

-spec add_edge(Schema::term(), V1::vertex(), Ext::assembly(), Label::term()) -> vertex().
add_edge(Schema, V1, Ext, Label) ->
    V2 = add_vertex(Schema, Ext),
    %% TODO: To determinate edge type (mesh or install);
    digraph:add_edge(Schema, V1, V2, Label),
    V2.

-spec del_path(Schema::term(), V1::vertex(), V2::vertex()) -> success().
del_path(Schema, V1, V2) ->
    true = digraph:del_path(Schema, V1, V2),
    erlmachine:success().

-spec add_vertex(Schema::term(), Ext::assembly()) -> vertex().
add_vertex(Schema, Ext) ->
    V = erlmachine_assembly:label(Ext),
    digraph:add_vertex(Schema, V, Ext),
    V.

-spec del_vertex(Schema::term(), V::vertex()) -> success().
del_vertex(Schema, V) ->
    true = digraph:del_vertex(Schema, V),
    erlmachine:success().

%% Can be used to retrive the full extensions list;
-spec vertices(Schema::term()) -> [] | [vertex()].
vertices(Schema) ->
    [vertex(Schema, V) || V <- digraph:vertices(Schema)].

%% We are going to provide access by path gearbox.shaft.# (like rabbitmq notation) too;
%% Can be used as find operation;
-spec vertex(Schema::term(), V::vertex()) -> 
                  assembly() | false.
vertex(Schema, V) ->
    case digraph:vertex(Schema, V) of 
        {_, Assembly} ->
            Assembly;
        _ ->
            false
    end.

-spec in_edges(Schema::term(), V::vertex()) -> [] | [edge()].
in_edges(Schema, V) ->
    [edge(Schema, E)|| E <- digraph:in_edges(Schema, V)].

%% Can be used to retrieve attachments list;
%% We should be able to filter this list accordingly to passed label argument (installed, meshed, etc.);
-spec edges(Schema::term()) -> [] | [edge()].
edges(Schema) ->
    [edge(Schema, E)|| E <- digraph:edges(Schema)].

-spec out_edges(Schema::term(), V::vertex()) -> [] | [edge()].
out_edges(Schema, V) ->
    [edge(Schema, E)|| E <- digraph:out_edges(Schema, V)].

-spec edge(Schema::term(), E::edge()) -> {assembly(), assembly()}.
edge(Schema, E) ->
    {_E, V1, V2, Label} = digraph:edge(Schema, E),
    {vertex(Schema, V1), vertex(Schema, V2), Label}.

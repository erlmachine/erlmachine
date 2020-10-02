-module(erlmachine_schema).
%% This module is responsible to setup the schema for the whole transmittion and to manage it;

-export([new/0]).

-export([add_edge/3]).
-export([add_vertex/2, del_vertex/2]).

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

-spec add_edge(GearBox::assembly(), V1::vertex(), Ext::assembly(), Label::term()) -> vertex().
add_edge(GearBox, V1, Ext, Label) ->
    V2 = add_vertex(GearBox, Ext),
    %% TODO: To determinate edge type (mesh or install);
    Schema = erlmachine_assembly:schema(GearBox),
    digraph:add_edge(Schema, V1, V2, Label),
    V2.

-spec del_path(GearBox::assembly(), V1::vertex(), V2::vertex()) -> success().
del_path(GearBox, V1, V2) ->
    Schema = erlmachine_assembly:schema(GearBox),
    true = digraph:del_path(Schema, V1, V2),
    erlmachine:success().

-spec add_vertex(GearBox::assembly(), Ext::assembly()) -> vertex().
add_vertex(GearBox, Ext) ->
    Schema = erlmachine_assembly:schema(GearBox),
    V = erlmachine_assembly:label(Ext),
    digraph:add_vertex(Schema, V, Ext),
    V.

-spec del_vertex(GearBox::term(), V::vertex()) -> success().
del_vertex(GearBox, V) ->
    Schema = erlmachine_assembly:schema(GearBox),
    true = digraph:del_vertex(Schema, V),
    erlmachine:success().

%% Can be used to retrive the full extensions list;
-spec vertices(GearBox::assembly()) -> [] | [vertex()].
vertices(GearBox) ->
    Schema = erlmachine_assembly:schema(GearBox),
    [vertex(GearBox, V) || V <- digraph:vertices(Schema)].

%% We are going to provide access by path gearbox.shaft.# (like rabbitmq notation) too;
%% Can be used as find operation;
-spec vertex(GearBox::assembly(), V::vertex()) -> 
                  assembly() | false.
vertex(GearBox, V) ->
    Schema = erlmachine_assembly:schema(GearBox),
    case digraph:vertex(Schema, V) of 
        {_, Assembly} ->
            Assembly;
        _ ->
            false
    end.

-spec in_edges(GearBox::assembly(), V::vertex()) -> [] | [edge()].
in_edges(GearBox, V) ->
    Schema = erlmachine_assembly:schema(GearBox),
    [edge(GearBox, E)|| E <- digraph:in_edges(Schema, V)].

%% Can be used to retrieve attachments list;
%% We should be able to filter this list accordingly to passed label argument (installed, meshed, etc.);
-spec edges(GearBox::assembly()) -> [] | [edge()].
edges(GearBox) ->
    Schema = erlmachine_assembly:schema(GearBox),
    [edge(GearBox, E)|| E <- digraph:edges(Schema)].

-spec out_edges(GearBox::assembly(), V::vertex()) -> [] | [edge()].
out_edges(GearBox, V) ->
    Schema = erlmachine_assembly:schema(GearBox),
    [edge(GearBox, E)|| E <- digraph:out_edges(Schema, V)].

-spec edge(GearBox::assembly(), E::edge()) -> {assembly(), assembly()}.
edge(GearBox, E) ->
    Schema = erlmachine_assembly:schema(GearBox),
    {_E, V1, V2, Label} = digraph:edge(Schema, E),
    {vertex(GearBox, V1), vertex(GearBox, V2), Label}.

-module(erlmachine_schema).
%% NOTE: This module is responsible to setup a graph based data structure;
%% TODO: To provide visualization of:

%% 1. Bootloading graph;
%% 2. Dataflow graph;
%% 3. Extension vetices which are labeled, tagged and decorated by desc;
%% 4. System report which covers extension state in a separate widjet;
%% 5. System report which covers node state in a separate widjet (erlang:memory/0)

-export([new/1]).

-export([add_vertex/3, del_vertex/2]).
-export([add_edge/4]).
-export([del_path/3]).

-export([vertex/2, vertices/1]).
-export([edge/2, edges/1]).
-export([in_edges/2, out_edges/2]).
-export([in_neighbours/2, out_neighbours/2]).

-export([graph/1]).
-export([meta/1]).
-export([description/1]).

-record(schema, {
                 %% The unique number which is assigned incrementally
                 id::integer(),
                 %% The common ancestor (bootloader)
                 mother::vertex(),
                 %% The schema contained structure
                 graph::graph(),
                 %% Layout customization (colors, styles, etc.)
                 meta::map(),
                 %% TODO Add location storage
                 %% The brief manufacturer overview
                 description::binary() 
                }).

-include("erlmachine_assembly.hrl").
-include("erlmachine_system.hrl").

-opaque graph() :: term().

-opaque vertex() :: term().
-opaque edge() :: term().

-export_type([graph/0, vertex/0, edge/0]).

%% TODO To simplify into graph?; To calculate mother via digraph_utils:topsort/1
%% Current implementation of a schema is digraph. But we are able to change it on demand;
-spec new(V::vertex()) -> schema().
new(V, Env) ->
    Graph = digraph:new(),
    #schema{ bootloader = V, graph = Graph }.

%%% Field accessors

-spec graph(Schema::schema()) -> term().
graph(Schema) ->
    Schema#schema.graph.

-spec graph(Schema::schema(), Graph::term()) -> schema().
graph(Schema, Graph) ->
    Schema#schema{ graph = Graph }.

%% TODO: Path can be specified #.reg, label.reg, etc..
%% Model doesn't have to know about meshed parts;
%% Model can skip rotation or to provide delivery path: #, #.reg, etc..
%% Passed extensions list automatically subscribed on # (relation is determined by edge);
%% Edge is described by reg.

%% NOTE: The main purpouse of this call is to register a route;
%% vertex/2 allows to find extension within schema (via label);
%% vertices/1 allows to query the full extensions list;

%%% Graph API

-spec add_vertex(Schema::schema(), V::vertex(), Ext::assembly()) -> vertex().
add_vertex(Schema, V, Ext) ->
    Graph = graph(Schema),
    digraph:add_vertex(Graph, V, Ext),
    V.

%% NOTE: The main purpouse of this call is to delete entry from the schema (routes also deleted);
-spec del_vertex(Schema::schema(), V::vertex()) -> success().
del_vertex(Schema, V) ->
    Graph = graph(Schema),
    true = digraph:del_vertex(Graph, V),
    erlmachine:success().

-spec add_edge(Schema::schema(), V1::vertex(), V2::vertex(), Label::term()) -> success().
add_edge(Schema, V1, V2, Label) ->
    Graph = graph(Schema),
    digraph:add_edge(Graph, V1, V2, Label),
    ok.

-spec del_path(Schema::schema(), V1::vertex(), V2::vertex()) -> success().
del_path(Schema, V1, V2) ->
    Graph = graph(Schema),
    true = digraph:del_path(Graph, V1, V2),
    ok.

-spec vertex(Schema::schema(), V::vertex()) -> 
                    assembly().
vertex(Schema, V) ->
    Graph = graph(Schema),
    {_, Assembly} = digraph:vertex(Graph, V),
    Assembly.

-spec vertices(Schema::schema()) -> [] | [vertex()].
vertices(Schema) ->
    Graph = graph(Schema),
    [vertex(Schema, V) || V <- digraph:vertices(Graph)].

-spec edge(Schema::schema(), E::edge()) -> {assembly(), assembly(), term()}.
edge(Schema, E) ->
    Graph = graph(Schema),
    {_E, V1, V2, Label} = digraph:edge(Graph, E),
    {vertex(Schema, V1), vertex(Schema, V2), Label}.

%% Can be used to retrieve attachments list;
%% We should be able to filter this list accordingly to passed label argument (installed, meshed, etc.);
-spec edges(Schema::schema()) -> [] | [edge()].
edges(Schema) ->
    Graph = graph(Schema),
    [edge(Schema, E)|| E <- digraph:edges(Graph)].

-spec in_edges(Schema::schema(), V::vertex()) -> [] | [edge()].
in_edges(Schema, V) ->
    Graph = graph(Schema),
    [edge(Schema, E)|| E <- digraph:in_edges(Graph, V)].

-spec out_edges(Schema::schema(), V::vertex()) -> [] | [edge()].
out_edges(Schema, V) ->
    Graph = graph(Schema),
    [edge(Schema, E)|| E <- digraph:out_edges(Graph, V)].

-spec in_neighbours(Schema::schema(), V::vertex()) -> [] | [vertex()].
in_neighbours(Schema, V) ->
    Graph = graph(Schema),
    [vertex(Schema, N)|| N <- digraph:in_neighbours(Graph, V)].

-spec out_neighbours(Schema::schema(), V::vertex()) -> [] | [vertex()].
out_neighbours(Schema, V) ->
    Graph = graph(Schema),
    [vertex(Schema, N)|| N <- digraph:out_neighbours(Graph, V)].

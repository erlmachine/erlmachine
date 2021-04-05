-module(erlmachine_graph).
%% NOTE: This module is responsible to setup a graph based data structure;
%% TODO: To provide visualization of:

%% 1. Bootloading graph;
%% 2. Dataflow graph;
%% 3. Extension vetices which are labeled, tagged and decorated by desc;
%% 4. System report which covers extension state within a separate widjet;
%% 5. System report which covers node state within a separate widjet (erlang:memory/0)

-export([new/0]).

-export([draw/1]).
-export([draw_vertex/2]).
-export([draw_edge/3]).

-export([add_vertex/3, del_vertex/2]).
-export([add_edge/4]).
-export([del_path/3]).

-export([vertex/2, vertices/1]).
-export([edge/2, edges/1]).
-export([in_edges/2, out_edges/2]).
-export([in_neighbours/2, out_neighbours/2]).

-export([topsort/1]).

-include("erlmachine_assembly.hrl").
-include("erlmachine_system.hrl").

-type graph() :: term().

-type vertex() :: term().
-type edge() :: term().

-export_type([graph/0, vertex/0, edge/0]).

%% TODO To simplify into graph?; To calculate mother via digraph_utils:topsort/1
%% Current implementation of a schema is digraph. But we are able to change it on demand;

%%% Constructor API

-spec new() -> graph().
new() ->
    digraph:new([acyclic]).

%%% Graph mapping

-spec draw(Assembly::assembly()) -> graph().
draw(Assembly) ->
    Graph = new(), ok = draw_vertex(Graph, Assembly),

    Exts = erlmachine_assembly:extensions(Assembly),
    ok = draw(Graph, Exts), [ok = draw_edge(Graph, Assembly, Ext) || Ext <- Exts],
    Graph.

draw(_Graph, []) ->
    ok;
draw(Graph, [H|T]) ->
    ok = draw_vertex(Graph, H),

    Exts = erlmachine_assembly:extensions(H),
    draw(Graph, Exts), [draw_edge(Graph, H, Ext) || Ext <- Exts],
    draw(Graph, T).

-spec draw_vertex(Graph::graph(), Assembly::assembly()) -> success().
draw_vertex(Graph, Assembly) ->
    V = erlmachine_assembly:vertex(Assembly), Rel = erlmachine_assembly:graph(Assembly, Graph),

    add_vertex(Graph, V, Rel).

-spec draw_edge(Graph::graph(), Assembly::assembly(), Ext::assembly()) -> success().
draw_edge(Graph, Assembly, Ext) ->
    Label = erlmachine_assembly:type(Assembly),
    V1 = erlmachine_assembly:vertex(Assembly), V2 = erlmachine_assembly:vertex(Ext),

    add_edge(Graph, V1, V2, Label).

%% NOTE: 1. Path can be specified #.reg, label.reg, etc..
%%       2. Model doesn't have to know about meshed parts;
%%       3. Model can skip rotation or to provide delivery path: #, #.reg, etc..
%%       4. Passed extensions list automatically subscribed on # (relation is determined by edge);
%%       5. Edge is described by reg.

%%% Access API
%% NOTE: The main purpouse of this call is to register a route;
-spec add_vertex(Graph::graph(), V::vertex(), Ext::assembly()) -> success().
add_vertex(Graph, V, Ext) ->
    Encode = erlmachine_assembly:to_vertex(Ext),
    digraph:add_vertex(Graph, V, Encode), 
    ok.

%% NOTE: The main purpouse of this call is to delete entry from the schema (routes also deleted);
-spec del_vertex(Graph::graph(), V::vertex()) -> success().
del_vertex(Graph, V) ->
    true = digraph:del_vertex(Graph, V),
    erlmachine:success().

-spec add_edge(Graph::graph(), V1::vertex(), V2::vertex(), Label::term()) -> success().
add_edge(Graph, V1, V2, Label) ->
    digraph:add_edge(Graph, V1, V2, Label),
    ok.

-spec del_path(Graph::graph(), V1::vertex(), V2::vertex()) -> success().
del_path(Graph, V1, V2) ->
    true = digraph:del_path(Graph, V1, V2),
    ok.

%% NOTE: vertex/2 allows to find extension within schema (via label);
-spec vertex(Graph::graph(), V::vertex()) -> 
                    assembly().
vertex(Graph, V) ->
    {_, Ext} = digraph:vertex(Graph, V),
    _Decode = erlmachine_assembly:from_vertex(Ext).

%% NOTE: vertices/1 allows to query the full extensions list;
-spec vertices(Graph::graph()) -> [vertex()].
vertices(Graph) ->
    [vertex(Graph, V) || V <- digraph:vertices(Graph)].

-spec edge(Graph::graph(), E::edge()) -> {assembly(), assembly(), term()}.
edge(Graph, E) ->
    {_E, V1, V2, Label} = digraph:edge(Graph, E),
    {vertex(Graph, V1), vertex(Graph, V2), Label}.

%% NOTE: Can be used to retrieve extensions list;
%% We should provide filter via passed label argument (installed, meshed, etc.);
-spec edges(Graph::graph()) -> [] | [edge()].
edges(Graph) ->
    [edge(Graph, E)|| E <- digraph:edges(Graph)].

-spec in_edges(Graph::graph(), V::vertex()) -> [] | [edge()].
in_edges(Graph, V) ->
    [edge(Graph, E)|| E <- digraph:in_edges(Graph, V)].

-spec out_edges(Graph::graph(), V::vertex()) -> [] | [edge()].
out_edges(Graph, V) ->
    [edge(Graph, E)|| E <- digraph:out_edges(Graph, V)].

-spec in_neighbours(Graph::graph(), V::vertex()) -> [] | [vertex()].
in_neighbours(Graph, V) ->
    [vertex(Graph, N)|| N <- digraph:in_neighbours(Graph, V)].

-spec out_neighbours(Graph::graph(), V::vertex()) -> [] | [vertex()].
out_neighbours(Graph, V) ->
    [vertex(Graph, N)|| N <- digraph:out_neighbours(Graph, V)].

%%% Utils

-spec topsort(Graph::graph()) -> [vertex()] | false.
topsort(Graph) ->
    digraph_utils:topsort(Graph).

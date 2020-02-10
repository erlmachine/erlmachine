-module(erlmachine_schema).

-export([vertex/2]).

-export([add_edges/2, add_edge/3]).

-export([add_vertex/2, del_vertex/2]).

-export([del_path/3]).

-include("erlmachine_factory.hrl").

%% Current implementation is digraph. But we are able to change it on demand;
%% We need to consider mounted field like indicator for of building mount topology; 

-spec add_edges(Schema::term(), Assembly::assembly()) -> term().
add_edges(Schema, Assembly) ->
    V = add_vertex(Schema, Assembly),
    add_edges(Schema, V, erlmachine_assembly:parts(Assembly)).

-spec add_edge(Schema::term(), V1::term(), Assembly::assembly()) -> term().
add_edge(Schema, V1, Assembly) ->
    V2 = add_vertex(Schema, Assembly),

    digraph:add_edge(Schema, V1, V2, []), 
    V2.

-spec del_path(Schema::term(), V1::term(), V2::term()) -> ok.
del_path(Schema, V1, V2) ->
    true = digraph:del_path(Schema, V1, V2),
    ok.

-spec add_vertex(Schema::term(), Assembly::assembly()) -> term().
add_vertex(Schema, Assembly) ->
    V = erlmachine_assembly:label(Assembly),
    digraph:add_vertex(Schema, V, Assembly),
    V.

-spec del_vertex(Schema::term(), V::term()) -> ok.
del_vertex(Schema, V) ->
    true = digraph:del_vertex(Schema, V), 
    ok.

-spec add_edges(Schema::term(), V1::term(), Parts::list(assembly())) -> 
                       term().
add_edges(_Schema, V1, []) ->
    V1;
add_edges(Schema, V1, [Part|T]) ->
    V2 = add_edge(Schema, V1, Part),

    add_edges(Schema, V2, erlmachine_assembly:parts(Part)),
    add_edges(Schema, V1, T), 
    V1.

-spec vertex(Schema::term(), V::term()) -> 
                  assembly() | false.
vertex(Schema, V) ->
    case digraph:vertex(Schema, V) of 
        {_, Assembly} ->
            Assembly;
        _ ->
            false
    end.

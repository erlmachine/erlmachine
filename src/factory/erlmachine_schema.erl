-module(erlmachine_schema).

-export([add_edges/2, add_edge/3, add_vertex/2, del_vertex/2, vertex/2]).

-include("erlmachine_factory.hrl").

%% Current implementation is digraph. But we are able to change it on demand;
%% We need to consider mounted field like indicator for of building mount topology; 

-spec add_edges(Schema::term(), Assembly::assembly()) -> term().
add_edges(Schema, Assembly) ->
    V1 = add_vertex(Schema, Assembly),
    add_edges(Schema, V1, erlmachine_assembly:parts(Assembly)).

-spec add_edge(Schema::term(), V1::serial_no(), Assembly::assembly()) -> term().
add_edge(Schema, V1, Assembly) ->
    V2 = add_vertex(Schema, Assembly),

    digraph:add_edge(Schema, V1, V2, []), 
    V2.

-spec add_vertex(Schema::term(), Assembly::assembly()) -> term().
add_vertex(Schema, Assembly) ->
    V1 = erlmachine_assembly:serial_no(Assembly),
    digraph:add_vertex(Schema, V1, Assembly),
    V1.

-spec del_vertex(Schema::term(), ID::serial_no()) -> ok.
del_vertex(Schema, ID) ->
    digraph:del_vertex(Schema, ID), 
    ok.

-spec add_edges(Schema::term(), V1::serial_no(), Parts::list(assembly())) -> term().
add_edges(_Schema, V1, []) ->
    V1;
add_edges(Schema, V1, [Part|T]) ->
    V2 = add_edge(Schema, V1, Part),

    add_edges(Schema, V2, erlmachine_assembly:parts(Part)),
    add_edges(Schema, V1, T), 
    V1.

-spec vertex(Schema::term(), SN::serial_no()) -> 
                  assembly() | false.
vertex(Schema, SN) ->
    case digraph:vertex(Schema, SN) of 
        {_V, Assembly} ->
            Assembly;
        _ ->
            false
    end.

-module(erlmachine_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

-export([start_phase/3]).

start(_Type, _Args) ->
    Nodes = [node()],
    mnesia:create_schema(Nodes), ok = mnesia:start(),

    _TrackTabRes = 
        mnesia:create_table(
          erlmachine_tracker:tabname(),
          [
           attributes(erlmachine_tracker:attributes()),
           disc_only_copies(Nodes),
           record_name(erlmachine_tracker:record_name())
          ]),

    _AssemblyTabRes = 
        mnesia:create_table(
          erlmachine_assembly:tabname(),
          [
           attributes(erlmachine_assembly:attributes()),
           record_name(erlmachine_assembly:record_name())
          ]),

    erlmachine_sup:start_link().


stop(_State) ->	
    Nodes = [node()],
    ok = mnesia:stop(), mnesia:delete_schema(Nodes).

start_phase(erlmachine_filesystem, _Type, _PhaseArgs) ->
    ok;
start_phase(_, _Type, _PhaseArgs) ->
    ok.

attributes(Attr) ->
    {attributes, Attr}.

disc_only_copies(Nodes) ->
    {disc_only_copies, Nodes}.

record_name(Name) ->
    {record_name, Name}.


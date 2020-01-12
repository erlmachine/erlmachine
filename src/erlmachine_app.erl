-module(erlmachine_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

-export([start_phase/3]).

start(_Type, _Args) ->
    Nodes = [node()],
    mnesia:create_schema(Nodes), ok = mnesia:start(),
    
    DiscOnlyCopies = {disc_only_copies, Nodes},
 
    TrackTab = erlmachine_tracker:table(), TrackAttr = erlmachine_tracker:attributes(),
    TrackTabTabDef = [{attributes, TrackAttr}, DiscOnlyCopies, {record_name, TrackTab}],

    mnesia:create_table(TrackTab, TrackTabTabDef),

    AssemblyTab = erlmachine_assembly:table(), AssemblyAttr = erlmachine_assembly:attributes(),
    AssemblyTabDef = [{attributes, AssemblyAttr}, DiscOnlyCopies, {record_name, AssemblyTab}],

    mnesia:create_table(AssemblyTab, AssemblyTabDef),

    erlmachine_sup:start_link().


stop(_State) ->	erlmachine:success().

start_phase(erlmachine_filesystem, _Type, _PhaseArgs) ->
    ok;
start_phase(_, _Type, _PhaseArgs) ->
    ok.

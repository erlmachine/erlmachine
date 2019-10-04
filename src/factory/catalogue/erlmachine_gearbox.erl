-module(erlmachine_gearbox).
-behaviour(gen_server).
%% Gearbox is a component which responsible for reliable spatial placement for all processes;
%% Gearbox is the place where shafts, gears and axles are fixed. 
%% Gearbox is the main container component
%% The gearbox is divided on so called stages (stage is a torgue between two independent gears);

-include("erlmachine_factory.hrl").

%% API.
-export([]).

-record(gearbox, {
                  input::assembly(),
                  parts=[]::list(assembly()),
                  placement::term(),
                  %% Placement can be implemented by various ways and then represented by different formats; 
                  %% Each implementation can do that over its own discretion;
                  %% Erlmachine do that accordingly to YAML format;
                  output::assembly()
                 }
       ).

-export_type gearbox()::#gearbox{}.

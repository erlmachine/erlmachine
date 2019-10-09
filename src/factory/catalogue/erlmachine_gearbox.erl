-module(erlmachine_gearbox).

%% The main puprouse of a product module is to provide API between clients part and system; 

%% Gearbox is a component which responsible for reliable spatial placement for all processes;
%% Gearbox is the place where shafts, gears and axles are fixed. 
%% Gearbox is the main container component
%% The gearbox is divided on so called stages (stage is a torgue between two independent gears);

-include("erlmachine_factory.hrl").
-include("erlmachine_system.hrl").

%% API.
-export([]).

-record(gearbox, {
                  input::assembly(),
                  body::term(),
                  parts=[]::list(assembly()),
                  placement::term(),
                  %% Placement can be implemented by various ways and then represented by different formats; 
                  %% Each implementation can do that over its own discretion;
                  %% Erlmachine do that accordingly to YAML format;
                  specs=[]::list(map()),
                  output::assembly()
                 }
       ).

-type gearbox() :: #gearbox{}.

-export_type([gearbox/0]).

%% detached;
%% (Reason == normal) orelse erlmachine_system:crash(Assembly, Reason),

%% erlmachine_system:damage(Assembly, Failure).

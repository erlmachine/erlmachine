-module(erlmachine_gearbox).
-behaviour(gen_server).
%% Gearbox is a component which responsible for reliable spatial placement for all processes;
%% Gearbox is the place where shafts, gears and axles are fixed. 
%% Gearbox is the main container component
%% The gearbox is divided on so called stages (stage is a torgue between two independent gears);

%% API.
-export([]).

-record(gearbox, {mount=[]::list()}).

-export_type gearbox()::#gearbox{}.

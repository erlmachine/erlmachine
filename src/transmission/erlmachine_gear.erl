-module(erlmachine_gear).
-export([]).

%% The main difference between gear and shaft is the next - gear as working element, shaft is transmitter instead; 
-record(gear, {body::term(), drive::gear()|shaft()}).

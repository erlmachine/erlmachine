-module(erlmachine_gear).

-include("erlmachine_factory.hrl").
-export([]).

%% The main difference between gear and shaft in the next - gear as working element, shaft is transmitter instead; 
-record(gear, {body::term(), drive::assembly()}).

-export_type gear()::#gear{}.

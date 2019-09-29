-module(erlmachine_axle).
-export([]).

-record(axle, {mount=[]::list()}).

-export_type axle()::#axle{}.

-module(erlmachine_axle).
-export([]).

-record(axle, {spline=[]::list()}).

-export_type axle()::#axle{}.

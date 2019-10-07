-module(erlmachine_axle).
-export([]).

-record(axle, {parts=[]::list(assembly()), mount::assembly()}).

-export_type axle()::#axle{}.

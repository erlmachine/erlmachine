-module(erlmachine_shaft).
-export([]).

%% Instead of gear the main puropse of shaft is to transmit power between parts;
-record(shaft, {body::term(), parts=[]::list(assembly()), mount::assembly()}.

-export_type shaft()::#shaft{}.

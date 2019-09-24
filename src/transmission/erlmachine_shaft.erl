-module(erlmachine_shaft).
-export([]).

%% Instead of gear the main puropse of shaft is to transmit power between parts;
-record(shaft, {body::term(), drive=[]::list()}).

-export_type shaft()::#shaft{}.

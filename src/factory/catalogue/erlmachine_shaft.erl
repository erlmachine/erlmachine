-module(erlmachine_shaft).

-include("erlmachine_factory.hrl").
-include("erlmachine_system.hrl").

-export([]).

%% Instead of gear the main puropse of shaft is to transmit power between parts;
-record(shaft, {body :: term(), parts=[] :: list(assembly()), mount :: assembly()}).

-type shaft() :: #shaft{}.

-export_type([shaft/0]).

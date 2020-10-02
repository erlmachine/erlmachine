-module(erlmachine_worker_sample_prototype).
-export([name/0]).

-spec name() -> Name::atom().
name() ->
    ?MODULE.

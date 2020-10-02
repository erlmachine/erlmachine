-module(erlmachine_worker).

-export([ack/1]).
-export([publish/1]).

-spec ack(MFArgs::mafargs()) -> success(assembly()) | failure(term(), term(), assembly()).
ack(_MFArgs) ->
    erlmachine:success([]).

-spec publish(Args::term()) -> success(assembly()) | failure(term(), term(), assembly()).
publish(_Args) ->
    erlmachine_success([]).

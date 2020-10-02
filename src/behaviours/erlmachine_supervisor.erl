-module(erlmachine_supervisor).

-export([ack/1]).

-include("erlmachine_factory.hrl").
-include("erlmachine_system.hrl").

-callback install(Label::term(), Ids::list(term()), Opt::term(), Env::list()) -> 
    success(term()) | failure(term(), term(), term()).

-callback install(Label::term(), Id::term()) -> 
    success(term()) | failure(term(), term(), term()).

-callback uninstall(Label::term(), Id::term()) -> 
    success(term()) | failure(term(), term(), term()).

-callback uninstall(Label::term()) ->
    success(term()) | failure(term(), term(), term()).

-spec ack(MFArgs::mafargs()) -> success() | failure(term(), term()).
ack(_MFArgs) ->
    erlmachine:success().

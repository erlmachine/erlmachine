-module(erlmachine_supervisor).

-include("erlmachine_assembly.hrl").
-include("erlmachine_system.hrl").

-callback install(Label::term(), Ids::list(term()), Opt::term(), Env::list()) -> 
    success(term()) | failure(term(), term(), term()).

-callback install(Label::term(), Id::term()) -> 
    success(term()) | failure(term(), term(), term()).

-callback uninstall(Label::term(), Id::term()) -> 
    success(term()) | failure(term(), term(), term()).

-callback uninstall(Label::term()) ->
    success(term()) | failure(term(), term(), term()).

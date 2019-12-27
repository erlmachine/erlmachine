-module(axle_tracker).

-folder(<<"erlmachine/mechanics/axle_tracker">>).

-behaviour(erlmachine_axle).

-accept([test]).

-export([
         install/5,
         uninstall/3,
         accept/3,
         attach/4, detach/3
        ]).

-include_lib("erlmachine/include/erlmachine_factory.hrl").
-include_lib("erlmachine/include/erlmachine_system.hrl").

-spec install(SN::serial_no(), IDs::list(serial_no()), State::map(), Options::term(), Env::list()) -> 
                     success(term()) | failure(term(), term(), term()) | failure(term()).
install(_SN, _IDs, State, _Options, _Env) ->
    {ok, State}.

-spec uninstall(SN::serial_no(), Reason::term(), State::map()) -> 
                       success(term()) | failure(term(), term(), term()) | failure(term()).
uninstall(_SN, _Reason, State) ->
    {ok, State}.

-spec accept(SN::serial_no(), Criteria::term(), State::map()) -> 
                    success(term(), term()) | failure(term(), term(), term()) | failure(term()).
accept(_SN, _Criteria, State) ->
    {ok, [], State}.

-spec attach(SN::serial_no(), Register::term(), ID::serial_no(), State::term()) -> 
                     success(term()) | failure(term(), term(), term()) | failure(term()).
attach(_SN, _Register, _ID, State) ->
    {ok, State}.

-spec detach(SN::serial_no(), ID::serial_no(), State::term()) -> 
                    success(term()) | failure(term(), term(), term()) | failure(term()).
detach(_SN, _ID, State) ->
    {ok, State}.

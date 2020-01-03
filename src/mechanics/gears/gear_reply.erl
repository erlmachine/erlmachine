-module(gear_reply).

-behaviour(erlmachine_gear).

-accept([test]).

-export([
         install/5,
         rotate/3, load/3, transmit/3,
         attach/4, detach/3,
         overload/3, block/4,
         replace/3,
         uninstall/3,
         accept/3
        ]).

-include_lib("erlmachine/include/erlmachine_factory.hrl").
-include_lib("erlmachine/include/erlmachine_system.hrl").

-spec install(SN::serial_no(), ID::serial_no(), State::map(), Opt::term(), Env::list()) ->
          success(term()) | failure(term(), term(), term()) | failure(term()).
install(_SN, _ID, State, _Opt, _Env) ->
     {ok, State}.

-spec replace(SN::serial_no(), ID::serial_no(), State::map()) ->
          success(term()) | failure(term(), term(), term()) | failure(term()).
replace(_SN, _ID, State) ->
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

-spec rotate(SN::serial_no(), Motion::map(), State::map()) ->
          success(term(), term()) | failure(term(), term(), term()) | failure(term()).
rotate(_SN, Motion, State) ->
    Header = erlmachine:header(Motion), Body = erlmachine:body(Motion),

    Adress = maps:get(return_address, Header, undefined),
 
    if is_pid(Adress) -> 
            erlang:send(Adress, Body),
            {ok, erlmachine:event(Header, #{send => Adress}), State};
       true  ->
            {ok, State} 
    end.

-spec load(SN::serial_no(), Load::term(), State::map()) ->
          success(term()) | success(term(), term()) | failure(term(), term(), term()) | failure(term()).
load(_SN, _Load, State) ->
    {ok, State}.

-spec transmit(SN::serial_no(), Motion::map(), State::map()) ->
          success(term(), term()) | failure(term(), term(), term()) | failure(term()).
transmit(_SN, _Motion, State) ->
    {ok, ignore, State}.

-spec overload(SN::serial_no(), Load::term(), State::map()) ->
          success(term()) | failure(term(), term(), term()) | failure(term()).
overload(_SN, _Load, State) ->
    {ok, State}.

-spec block(SN::serial_no(), ID::serial_no(), Failure::term(), State::map()) ->
          success(term()) | failure(term(), term(), term()) | failure(term()).
block(_SN, _ID, _Failure, State) ->
    {ok, State}.

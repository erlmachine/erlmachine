-module(gear_mnesia).

-folder(<<"erlmachine/mechanics/gear_mnesia">>).

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
install(_SN, _ID, State, Opt, _Env) ->
    Name = proplists:get_value(name, Opt), TabDef = proplists:get_value(tabdef, Opt),
    Lock = proplists:get_value(lock, Opt, write),
    io:format("~nName: ~p TabDef: ~p~n",[Name, TabDef]),
    R = mnesia:create_table(Name, TabDef), io:format("~nR: ~p~n",[R]),
    
    {ok, State#{ name => Name, lock => Lock }}.

-spec replace(SN::serial_no(), ID::serial_no(), Body::map()) -> 
                     success(term()) | failure(term(), term(), term()) | failure(term()).
replace(SN, ID, State) ->
    io:format("~n~p:replace(~p, ~p, ~p)~n",[?MODULE, SN, ID, State]),
    {ok, State}.

-spec uninstall(SN::serial_no(), Reason::term(), State::map()) -> 
                       success(term()) | failure(term(), term(), term()) | failure(term()).
uninstall(SN, Reason, State) ->
    io:format("~n~p:uninstall(~p, ~p, ~p)~n",[?MODULE, SN, Reason, State]),
    {ok, State}.

-spec accept(SN::serial_no(), Criteria::term(), State::map()) -> 
                    success(term(), term()) | failure(term(), term(), term()) | failure(term()).
accept(SN, Criteria, State) ->
    io:format("~n~p:accept(~p, ~p, ~p)~n",[?MODULE, SN, Criteria, State]),
    {ok, [], State}.

-spec attach(SN::serial_no(), Register::term(), ID::serial_no(), State::term()) -> 
                    success(term()) | failure(term(), term(), term()) | failure(term()).
attach(SN, Register, ID, State) ->
    io:format("~n~p:attach(~p, ~p, ~p, ~p)~n",[?MODULE, SN, Register, ID, State]),
    {ok, State}.

-spec detach(SN::serial_no(), ID::serial_no(), State::term()) -> 
                    success(term()) | failure(term(), term(), term()) | failure(term()).
detach(SN, ID, State) ->
    io:format("~n~p:detach(~p, ~p, ~p)~n",[?MODULE, SN, ID, State]),
    {ok, State}.

-spec rotate(SN::serial_no(), Motion::map(), State::map()) -> 
                     success(term(), term()) | failure(term(), term(), term()) | failure(term()).
rotate(_SN, Motion, #{ name := Name, lock := _Lock }=State) ->
    Header = erlmachine:header(Motion), Body = erlmachine:body(Motion),
    Message  =
        try 
            [Command] = maps:keys(Body), Args = maps:get(Command, Body),
            %% TODO add transaction support;
            case Command of 
                write ->
                    is_tuple(Args) orelse throw(?LINE),
                    
                    ok = mnesia:dirty_write(Args),
                    Event = #{ name => Name, id => element(1, Args) },
                    erlmachine:event(Header, #{ ?MODULE => Event });
                read -> 
                    Result = mnesia:dirty_read(Name, Args),
                    erlmachine:document(Header, #{ Name => Result }) 
            end
        catch E:R ->
                io:format("~nE: ~p R: ~p~n",[E, R]),
                erlmachine:event(Header, #{ ?MODULE => erlmachine:failure(E, R) })
        end,
    {ok, Message, State}.

-spec load(SN::serial_no(), Load::term(), State::map()) -> 
                  success(term()) | success(term(), term()) | failure(term(), term(), term()) | failure(term()).
load(SN, Load, State) ->
    io:format("~n~p:load(~p, ~p, ~p)~n",[?MODULE, SN, Load, State]),
    {ok, State}.

-spec transmit(SN::serial_no(), Motion::map(), State::map()) -> 
                      success(term(), term()) | failure(term(), term(), term()) | failure(term()).
transmit(SN, Motion, State) ->
    io:format("~n~p:transmit(~p, ~p, ~p)~n",[?MODULE, SN, Motion, State]),
    {ok, ignore, State}.

-spec overload(SN::serial_no(), Load::term(), State::map()) -> 
                      success(term()) | failure(term(), term(), term()) | failure(term()).
overload(SN, Load, State) ->
    io:format("~n~p:overload(~p, ~p, ~p)~n",[?MODULE, SN, Load, State]),
    {ok, State}.

-spec block(SN::serial_no(), ID::serial_no(), Failure::term(), State::map()) -> 
                   success(term()) | failure(term(), term(), term()) | failure(term()).
block(SN, ID, Failure, State) ->
    io:format("~n~p:block(~p, ~p, ~p, ~p)~n",[?MODULE, SN, ID, Failure, State]),
    {ok, State}.


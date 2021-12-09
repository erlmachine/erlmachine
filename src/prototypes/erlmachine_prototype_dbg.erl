-module(erlmachine_prototype_dbg).

-behaviour(gen_server).
-behaviour(erlmachine_worker_prototype).

-export([name/0]).

%% erlmachine_worker_prototype.
-export([prototype_init/3]).
-export([prototype_call/2]).
-export([prototype_cast/2]).
-export([prototype_terminate/3]).

%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).

-include("erlmachine_factory.hrl").
-include("erlmachine_system.hrl").

-spec name() -> atom().
name() ->
    ?MODULE.

-spec id(SN::serial_no()) -> atom().
id(SN) ->
    binary_to_atom(SN).

%%%  erlmachine_worker_prototype behaviour

-record(init, { context::term(), opt::map() }).

-spec prototype_init(SN::serial_no(), Context::term(), Opt::map()) ->
                            success(pid()) | failure(term(), term()).
prototype_init(SN, Context, Opt) ->
    Com = #init{ context = Context, opt = Opt },

    Res = gen_server:start_link({local, id(SN)}, ?MODULE, Com, []),
    Res.

-record(call, { request::term() }).

-spec prototype_call(SN::serial_no(), Req::term()) ->
                            term().
prototype_call(SN, Req) ->
    Com = #call{ request = Req },

    Res = gen_server:call(id(SN), Com),
    Res.

-record(cast, { message::term() }).

-spec prototype_cast(SN::serial_no(), Msg::term()) ->
                            success().
prototype_cast(SN, Msg) ->
    Com = #cast{ message = Msg },

    Res = gen_server:cast(id(SN), Com),
    Res.

-spec prototype_terminate(SN::serial_no(), Reason::term(), Timeout::term()) ->
                                 success().
prototype_terminate(SN, Reason, Timeout) ->
    Res = gen_server:stop(id(SN), Reason, Timeout),
    Res.

%%%  gen_server behaviour

-record(state, { context::term() }).

init(#init{ context = Context, opt = Opt }) ->
    ok = dbg("~n~p:init(~p, ~p)~n", [?MODULE, Context, Opt]),
    ok = dbg("~n~p~n", [Context]),

    erlang:process_flag(trap_exit, true),
    {ok, Context2} = erlmachine_worker_prototype:init(Context),

    {ok, #state{ context = Context2 }}.

handle_call(#call{ request = Req }, _From, #state{ context = Context}) ->
    ok = dbg("~n~p:handle_call(~p)~n", [?MODULE, Req]),
    ok = dbg("~n~p~n", [Context]),

    {ok, Res, Context2} = erlmachine_worker_prototype:call(Context, Req),
    {reply, Res, #state{ context = Context2 }};

handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

handle_cast(#cast{ message = Msg }, #state{ context = Context }) ->
    ok = dbg("~n~p:handle_cast(~p)~n", [?MODULE, Msg]),
    ok = dbg("~n~p~n", [Context]),

    {ok, Context2} = erlmachine_worker_prototype:cast(Context, Msg),
    {noreply, #state{ context = Context2 }}.

handle_info(Info, #state{ context = Context }) ->
    ok = dbg("~n~p:handle_info(~p)~n", [?MODULE, Info]),
    ok = dbg("~n~p~n", [Context]),

    {ok, Context2} = erlmachine_worker_prototype:info(Context, Info),
    {noreply, #state{ context = Context2 }}.

terminate(Reason, #state{ context = Context }) ->
    ok = dbg("~n~p:terminate(~p)~n", [?MODULE, Reason]),
    ok = dbg("~n~p~n", [Context]),

    erlmachine_worker_prototype:terminate(Context, Reason),
    ok.

%%% Trace

-spec dbg(T::list(), Args::[term()]) -> success().
dbg(T, Args) ->
    io:format(user, T, Args),
    ok.

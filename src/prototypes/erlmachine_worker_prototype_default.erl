-module(erlmachine_worker_prototype_default).
-export([name/0]).

%% API.
-export([prototype_init/3]).
-export([prototype_call/2]).
-export([prototype_cast/2]).
-export([prototype_terminate/1]).

%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).

-behaviour(gen_server).
-behaviour(erlmachine_worker_prototype).

-include("erlmachine_factory.hrl").
-include("erlmachine_system.hrl").

-spec name() -> atom().
name() ->
    ?MODULE.

-spec id(SN::serial_no()) -> atom().
id(SN) ->
    erlang:binary_to_atom(SN, latin1).

%%%===================================================================
%%%  erlmachine_worker_prototype behaviour
%%%===================================================================

-record(init, { context::term(), opts::[] }).

-spec prototype_init(SN::serial_no(), Context::term(), Opts::list()) ->
                            success(pid()) | failure(term(), term()).
prototype_init(SN, Context, Opts) ->
    Com = #init{ context = Context, opts = Opts },
    gen_server:start_link({local, id(SN)}, ?MODULE, Com, []).

-record(call, { request::term() }).

-spec prototype_call(SN::serial_no(), Req::term()) ->
                            term().
prototype_call(SN, Request) ->
    Com = #call{ request = Request },
    gen_server:call(id(SN), Com).

-record(cast, { message::term() }).

-spec prototype_cast(SN::serial_no(), Msg::term()) ->
                            success().
prototype_cast(SN, Msg) ->
    Com = #cast{ message = Msg },
    gen_server:cast(id(SN), Com).

-spec prototype_terminate(SN::serial_no()) ->
                                 success().
prototype_terminate(SN) ->
    gen_server:stop(id(SN)).

%%%===================================================================
%%%  gen_server behaviour
%%%===================================================================

-record(state, { context::term() }).

init(#init{ context = Context, opts = _Opts }) ->
    {ok, Context2} = erlmachine_worker_prototype:init(Context),

    {ok, #state{ context = Context2 }}.

handle_call(#call{ request = Req }, _From, #state{ context = Context}) ->
    {ok, Res, Context2} = erlmachine_worker_prototype:call(Context, Req),

    {reply, Res, #state{ context = Context2 }};

handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

handle_cast(#cast{ message = Msg }, #state{ context = Context }) ->
    {ok, Context2} = erlmachine_worker_prototype:cast(Context, Msg),

    {noreply, #state{ context = Context2 }}.

handle_info(Info, #state{ context = Context }) ->
    {ok, Context2} = erlmachine_worker_prototype:info(Context, Info),

    {noreply, #state{ context = Context2 }}.

terminate(Reason, #state{ context = Context }) ->
    erlmachine_worker_prototype:terminate(Context, Reason),
    ok.

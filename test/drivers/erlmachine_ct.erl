-module(erlmachine_ct).

-behaviour(gen_server).

%% API.
-export([start/1, stop/0]).

-export([boot/0]).
-export([install/1, uninstall/1]).
-export([execute/2]).
-export([shutdown/0]).

%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).

-include("erlmachine_factory.hrl").
-include("erlmachine_assembly.hrl").
-include("erlmachine_system.hrl").

id() ->
    ?MODULE.

-spec start(Env::map()) -> success(pid()) | ingnore | failure(term()).
start(Env) ->
    gen_server:start({local, id()}, ?MODULE, Env, []).

-record (boot, { }).

-spec boot() -> success(pid()) | failure(term(), term()).
boot() ->
    gen_server:call(id(), #boot{}).

-record(install, { extension::assembly() }).

-spec install(Ext::assembly()) -> success(pid()) | failure(term(), term()).
install(Ext) ->
    gen_server:call(id(), #install{ extension = Ext }).

-record(uninstall, { vertex::term() }).

-spec uninstall(V::term()) -> success().
uninstall(V) ->
    gen_server:call(id(), #uninstall{ vertex = V }).

-record(execute, { vertex::term(), command::term() }).

-spec execute(V::term(), Command::term()) -> term().
execute(V, Command) ->
    gen_server:call(id(), #execute{ vertex = V, command = Command }).

-record (shutdown, { }).

-spec shutdown() -> success().
shutdown() ->
    gen_server:call(id(), #shutdown{}).

%% TODO: To provide transmission API;

-spec stop() -> success().
stop() ->
    gen_server:stop(id()).

%%%===================================================================
%%% gen_server behaviour
%%%===================================================================

-record(state, { schema::term() }).

init(Env) ->
    Ext = erlmachine_factory:gear(erlmachine_worker_ct, [], ['test', 'ct']),
    GearBox = erlmachine_factory:gearbox(erlmachine_supervisor_ct, [], Env, ['ct'], [Ext]),
    Schema = erlmachine_assembly:schema(GearBox),
    {ok, #state{ schema = Schema }}.


handle_call(#boot{}, _From, #state{ schema = Schema } = State) ->
    Res = erlmachine:boot(Schema),

    {reply, Res, State};

handle_call(#install{ extension = Ext }, _From, #state{ schema = Schema } = State) ->
    Res = erlmachine:install(Schema, Ext),

    {reply, Res, State};

handle_call(#uninstall{ vertex = V }, _From, #state{ schema = Schema } = State) ->
    Res = erlmachine:uninstall(Schema, V),

    {reply, Res, State};

handle_call(#execute{ vertex = V, command = Command }, _From, #state{ schema = Schema } = State) ->
    Res = erlmachine:execute(Schema, V, Command),

    {reply, Res, State};

handle_call(#shutdown{}, _From, #state{ schema = Schema } = State) ->
    Res = erlmachine:shutdown(Schema),

    {reply, Res, State};

handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
    ok.

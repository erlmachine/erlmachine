-module(erlmachine).
%% NOTE: The main purpouse of erlmachine project is to provide a set of well designed behaviours which are supported by visual tools (flowcharts, widjets, etc..).
%% The Erlmachine based design operates via flexible mechanism of prototypes and models. Where business layer is decoupled from transport implementation.

-export([start/0, stop/0, get_key/1, get_key/2]).

-export(priv_dir/0).

-export([is_supervisor/1, is_worker/1]).

-export([startup/1, startup/2]).

-export([process/3]).
-export([execute/3]).
-export([install/3, uninstall/3]).

-export([shutdown/2, shutdown/3]).

-export([motion/2]).
-export([header/1, header/2, body/1, body/2]).

-export([command/2, command/3]).
-export([document/2, document/3]).
-export([event/2, event/3]).
-export([command_name/1, document_meta/1, event_type/1]).

-export([request/2, request/3]).

-export([return_address/1, return_address/2]).
-export([correlation_id/1, correlation_id/2]).

-export([reply/2]).

-export([failure/0, failure/1, failure/2, failure/3]).
-export([success/0, success/1, success/2]).
-export([is_success/1, is_failure/1]).

-export([attributes/1]).
-export([behaviours/1]).
-export([optional_callback/3, optional_callback/4]).

-export([serial_no/1]).
-export([graph/1]).
-export([vertex/1, vertex/2]).
-export([tags/1]).
-export([part_no/1]).
-export([description/1]).

-export([guid/1]).

-export([md5/1]).
-export([phash2/1]).
-export([base64url/1]).

-export([tag/2, untag/2]).

-export([timestamp/0]).

-include("erlmachine_system.hrl").
-include("erlmachine_assembly.hrl").
-include("erlmachine_factory.hrl").

-type graph() :: erlmachine_graph:graph().
-type vertex() :: erlmachine_graph:vertex().

-type motion() :: erlmachine_transmission:motion().
-type header() :: erlmachine_transmission:header().
-type body() :: erlmachine_transmission:body().

-record(guid, { node::node(), reference::reference(), serial::term() }).

-type guid()::#guid{}.

-export_type([guid/0]).

%%% Application API

-spec start() -> success().
start() ->
    {ok, _} = application:ensure_all_started(?MODULE),
    success().

-spec stop() -> success() | failure(Reason :: any()).
stop() ->
    application:stop(?MODULE).

-spec get_key(Key::atom()) -> undefined | success(Val).
get_key(Key) ->
    get_key(?MODULE, Key).

-spec get_key(Application::atom(), Key::atom()) -> undefined | success(Val).
get_key(Application, Key) ->
    application:get_key(Application, Key).

-spec priv_dir() -> file:filename().
priv_dir() ->
    code:priv_dir(?MODULE).

-spec is_supervisor(Assembly::assembly()) -> boolean().
is_supervisor(Assembly) ->
    Type = erlmachine_assembly:type(Assembly),
    Type == 'supervisor'.

-spec is_worker(Assembly::assembly()) -> boolean().
is_worker(Assembly) ->
    Type = erlmachine_assembly:type(Assembly),
    Type == 'worker'.

-spec startup(Assembly::assembly()) ->
                   success(pid()) | ingnore | failure(term()).
startup(Assembly) ->
    erlmachine_transmission:startup(Assembly).

-spec startup(Graph::graph(), V::vertex()) ->
                   success(pid()) | ingnore | failure(term()).
startup(Graph, V) ->
    erlmachine_transmission:startup(Graph, V).

-spec process(Graph::graph(), V::vertex(), Motion::term()) ->
                    term().
process(Graph, V, Motion) ->
    erlmachine_transmission:process(Graph, V, Motion).

-spec execute(Graph::graph(), V::vertex(), Command::term()) ->
                      term().
execute(Graph, V, Command) ->
    erlmachine_transmission:execute(Graph, V, Command).

-spec install(Graph::graph(), V::vertex(), Ext::assembly()) ->
                     success(pid()) | ingnore | failure(term()).
install(Graph, V, Ext) ->
    erlmachine_transmission:install(Graph, V, Ext).

-spec uninstall(Graph::graph(), V::vertex(), Id::term()) ->
                       success().
uninstall(Graph, V, Id) ->
    erlmachine_transmission:uninstall(Graph, V, Id).

-spec shutdown(Graph::graph(), V::vertex()) ->
                  success().
shutdown(Graph, V) ->
    Reason = normal,
    shutdown(Graph, V, Reason).

-spec shutdown(Graph::graph(), V::vertex(), Reason::term()) ->
                      success().
shutdown(Graph, V, Reason) ->
    Timeout = 5000,
    erlmachine_transmission:shutdown(Graph, V, Reason, Timeout).

%%%  Message construction API

-spec motion(Header::header(), Body::body()) -> motion().
motion(Header, Body) ->
    erlmachine_transmission:motion(Header, Body).
%% NOTE: The assumtion is that header is exchenged in two way coordination (responder has to add extra fields);
-spec header(Motion::motion()) -> header().
header(Motion) ->
    erlmachine_transmission:header(Motion).

-spec header(Motion::motion(), Header::header()) -> motion().
header(Motion, Header) ->
    erlmachine_transmission:header(Motion, Header).

-spec body(Motion::motion()) -> body().
body(Motion) ->
    erlmachine_transmission:body(Motion).

-spec body(Motion::motion(), Body::body()) -> motion().
body(Motion, Body) ->
    erlmachine_transmission:body(Motion, Body).

-spec command(Name::term(), Args::body()) ->
                     motion().
command(Name, Args) ->
    Header = #{},
    command(Header, Name, Args).

-spec command(Header::header(), Name::term(), Args::body()) ->
                     motion().
command(Header, Name, Args) ->
    erlmachine_transmission:motion(Header#{ command => Name }, Args).

-spec document(Meta::term(), Body::body()) -> 
                      motion().
document(Meta, Body) ->
    Header = #{},
    document(Header, Meta, Body).

-spec document(Header::header(), Meta::term(), Body::body()) -> 
                      motion().
document(Header, Meta, Body) ->
    erlmachine_transmission:motion(Header#{ document => Meta }, Body).

-spec event(Type::term(), Description::body()) -> 
                   motion(). 
event(Type, Description) ->
    Header = #{},
    event(Header, Type, Description).

-spec event(Header::header(), Type::term(), Description::body()) -> 
                   motion(). 
event(Header, Type, Description) ->
    erlmachine_transmission:motion(Header#{ event => Type }, Description).

-spec command_name(Motion::motion()) -> term().
command_name(Motion) ->
    Header = header(Motion),
    maps:get(command, Header).

-spec document_meta(Motion::motion()) -> term().
document_meta(Motion) ->
    Header = header(Motion),
    maps:get(document, Header).

-spec event_type(Motion::motion()) -> term().
event_type(Motion) ->
    Header = header(Motion),
    maps:get(event, Header).

-spec request(Motion::motion(), Address::term()) -> 
                           motion().
request(Motion, Address) ->
    return_address(Motion, Address).

-spec request(Motion::motion(), Address::term(), Id::term()) -> 
                           motion().
request(Motion, Address, Id) ->
    correlation_id(return_address(Motion, Address), Id).

-spec return_address(Motion::motion()) -> term().
return_address(Motion) ->
    Header = header(Motion),
    maps:get(return_address, Header, undefined).

-spec return_address(Motion::motion(), Address::term()) -> motion().
return_address(Motion, Address) ->
    Header = header(Motion),
    header(Motion, Header#{ return_address => Address }).

-spec correlation_id(Motion::motion()) -> term().
correlation_id(Motion) ->
    Header = header(Motion),
    maps:get(correlation_id, Header, undefined).

-spec correlation_id(Motion::motion(), Id::term()) -> motion().
correlation_id(Motion, Id) ->
    Header = header(Motion),
    header(Motion, Header#{ correlation_id => Id }).

-spec reply(Req::motion(), Res::motion()) -> motion().
reply(Req, Res) ->
    Header = maps:merge(header(Req), header(Res)),
    header(Res, Header).

%%% Result API

-spec failure() -> failure().
failure() ->
    erlmachine_system:failure().

-spec failure(E::term()) -> failure(E::term()).
failure(E) ->
    erlmachine_system:failure(E).

-spec failure(E::term(), R::term()) -> failure(E::term(), R::term()).
failure(E, R) -> 
    erlmachine_system:failure(E, R).

-spec failure(E::term(), R::term(), S::term()) -> failure(E::term(), R::term(), S::term()).
failure(E, R, S) -> 
    erlmachine_system:failure(E, R, S).

-spec success(Result::term()) -> success(Result::term()).
success(Result) ->
    erlmachine_system:success(Result).

-spec success(Result::term(), State::term()) -> success(Result::term(), State::term()).
success(Result, State) -> 
    erlmachine_system:success(Result, State).

-spec success() -> success().
success() ->
    erlmachine_system:success().

-spec is_success(Res::term()) -> boolean().
is_success(Res) ->
    erlmachine_system:is_success(Res).

-spec is_failure(Res::term()) -> boolean().
is_failure(Res) ->
    erlmachine_system:is_failure(Res).

%%% Module API

-spec attributes(Module::atom()) -> [{atom(), term()}].
attributes(Module) ->
    Module:module_info(attributes).

-spec behaviours(Module::atom()) -> [atom()].
behaviours(Module) ->
    [Name|| {behaviour, Name} <- attributes(Module)].

-spec optional_callback(Mod::atom(), Fun::atom(), Args::list()) -> 
                               term().
optional_callback(Mod, Fun, Args) ->
    optional_callback(Mod, Fun, Args, success()).

-spec optional_callback(Mod::atom(), Fun::atom(), Args::list(), Def::term()) -> 
                               term().
optional_callback(Mod, Fun, Args, Def) ->
    case erlang:function_exported(Mod, Fun, length(Args)) of 
        true ->
            erlang:apply(Mod, Fun, Args); 
        _  -> 
            Def 
    end.

%%% Field accessors

-spec serial_no(Assembly::assembly()) -> serial_no().
serial_no(Assembly) ->
    erlmachine_assembly:serial_no(Assembly).

-spec graph(Assembly::assembly()) -> graph().
graph(Assembly) ->
    erlmachine_assembly:graph(Assembly).

-spec vertex(Assembly::assembly()) -> term().
vertex(Assembly) ->
    erlmachine_assembly:vertex(Assembly).

-spec vertex(Assembly::assembly(), Vertex::term()) -> assembly().
vertex(Assembly, Vertex) ->
    erlmachine_assembly:vertex(Assembly, Vertex).

-spec tags(Assembly::assembly()) -> list(). 
tags(Assembly) ->
    erlmachine_assembly:tags(Assembly).

-spec part_no(Assembly::assembly()) -> part_no().
part_no(Assembly) ->
    erlmachine_assembly:part_no(Assembly).

-spec description(Assembly::assembly()) -> binary().
description(Assembly) ->
    erlmachine_assembly:description(Assembly).

%% NOTE: This format is safer and more applicable by web applications (in comparison to base64);

-spec guid(Serial::term()) -> binary().
guid(Serial) ->
    GUID = #guid{ node=node(), serial=Serial, reference=make_ref() },
    md5(GUID).

-spec md5(Data::term()) -> binary().
md5(Data) ->
    MD5 = erlang:md5(term_to_binary(Data)),
    MD5.

-spec phash2(Term::term()) -> non_neg_integer().
phash2(Term) ->
    erlang:phash2(Term, 4294967296).

-spec base64url(N::binary()) -> Base64::binary().
base64url(N) when is_binary(N) ->
    Base64 = base64:encode(N),
    Base64Url = [fun($+) -> <<"-">>; ($/) -> <<"_">>; (C) -> <<C>> end(Char)|| <<Char>> <= Base64],
    << <<X/binary>> || X <- Base64Url >>.

-spec tag(Assembly::assembly(), Tag::term()) -> assembly().
tag(Assembly, Tag) ->
    Tags = erlmachine_assembly:tags(Assembly),
    erlmachine_assembly:tags(Assembly, [Tag|Tags]).

-spec untag(Assembly::assembly(), Tag::term()) -> assembly().
untag(Assembly, Tag) ->
    Tags = erlmachine_assembly:tags(Assembly),
    erlmachine_assembly:tags(Assembly, lists:delete(Tag, Tags)).

-spec timestamp() -> integer().
timestamp() ->
    erlang:monotonic_time(second) + erlang:time_offset(second).

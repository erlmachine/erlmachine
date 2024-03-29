-module(erlmachine).

%% NOTE The main purpouse of erlmachine project is to provide a set of well designed behaviours
%% NOTE They should be supported by visual tools (flowcharts, widjets, etc.)

%% NOTE Erlmachine based design operates via flexible mechanism of prototypes and models
%% NOTE They are designed to decouple a business layer (model) from service (prototype) implementation

%% TODO Erlanchine has replicated topology graph which is based on https://github.com/rabbitmq/ra

-export([init/0, init/1]).

-export([get_key/1, get_key/2, priv_dir/0, filename/1]).

-export([modules/0]).
-export([vsn/0]).
-export([description/0]).

-export([is_supervisor/1, is_worker/1]).

-export([startup/1, startup/2]).

-export([process/3]).
-export([execute/3]).
-export([install/2, install/3]).
-export([uninstall/2, uninstall/3]).

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
-export([history/1, history/2]).

-export([reply/2]).

-export([failure/0, failure/1, failure/2, failure/3]).
-export([success/0, success/1, success/2, success/3]).
-export([is_success/1, is_failure/1]).

-export([attributes/1]).
-export([behaviours/1]).
-export([optional_callback/3, optional_callback/4]).
-export([vsn/1]).

-export([serial_no/1]).
-export([model_no/1]).
-export([port/1, port/2]).
-export([graph/1]).
-export([uid/1]).
-export([tags/1, tags/2]).
-export([vertex/1, vertex/2]).
-export([part_no/1]).
-export([description/1]).

-export([guid/1]).

-export([md5/1]).
-export([phash2/1]).
-export([base64url/1]).

-export([tag/2, untag/2]).

-export([timestamp/0]).

-include_lib("erlbox/include/erlbox.hrl").

-include("erlmachine_user.hrl").
-include("erlmachine_assembly.hrl").
-include("erlmachine_factory.hrl").

-type graph() :: erlmachine_graph:graph().
-type vertex() :: erlmachine_graph:vertex().

-type motion() :: erlmachine_transmission:motion().
-type header() :: erlmachine_transmission:header().
-type body() :: erlmachine_transmission:body().

-record(guid, { node::node(), reference::reference(), serial::term() }).

-type guid() :: #guid{}.

-export_type([guid/0]).

%%% Behaviours API

-spec init() -> success().
init() ->
    Modules = modules(),
    ok = init(Modules).

-spec init(Modules::[module()]) -> success().
init(Modules) ->
    Tables = erlmachine_table:tables(Modules),
    [ok = erlmachine_table:create(T) || T <- Tables],

    Templates = erlmachine_template:templates(Modules),
    [ok = erlmachine_template:add_schema(T) || T <- Templates],

    Scopes = erlmachine_scope:scopes(Modules),
    Scopes == [] orelse erlmachine_scope:init(Scopes),

    success().

%%% Application API

-spec get_key(Key::atom()) -> 'undefined' | success(term()).
get_key(Key) ->
    get_key(?MODULE, Key).

-spec get_key(App::module(), Key::atom()) -> 'undefined' | success(term()).
get_key(App, Key) ->
    application:get_key(App, Key).

-spec priv_dir() -> file:filename().
priv_dir() ->
    code:priv_dir(?MODULE).

-spec filename(Path::list()) -> list().
filename(Path) ->
    filename:join(priv_dir(), Path).

-spec modules() -> [module()].
modules() ->
    {ok, Modules} = get_key(?MODULE, 'modules'),
    Modules.

-spec vsn() -> binary().
vsn() ->
    {ok, Vsn} = get_key(?MODULE, 'vsn'),
    Vsn.

-spec description() ->  binary().
description() ->
    {ok, Desc} = get_key(?MODULE, 'description'),
    Desc.

%%% Transmission API

-spec startup(Graph::graph()) ->
                   success(pid()) | ingnore | failure(term()).
startup(Graph) ->
    [Root|_] = erlmachine_graph:topsort(Graph), startup(Graph, Root).

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

-spec install(Graph::graph(), Ext::assembly()) ->
                     success(pid()) | ingnore | failure(term()).
install(Graph, Ext) ->
    [Root|_] = erlmachine_graph:topsort(Graph), install(Graph, Root, Ext).

-spec install(Graph::graph(), V::vertex(), Ext::assembly()) ->
                     success(pid()) | ingnore | failure(term()).
install(Graph, V, Ext) ->
    erlmachine_transmission:install(Graph, V, Ext).

-spec uninstall(Graph::graph(), V2::vertex()) ->
                       success().
uninstall(Graph, V2) ->
    [Root|_] = erlmachine_graph:topsort(Graph), uninstall(Graph, Root, V2).

-spec uninstall(Graph::graph(), V::vertex(), V2::vertex()) ->
                       success().
uninstall(Graph, V, V2) ->
    erlmachine_transmission:uninstall(Graph, V, V2).

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
%%% NOTE: https://www.enterpriseintegrationpatterns.com/patterns/messaging/MessageConstructionIntro.html;

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

%%% Command message
%% https://www.enterpriseintegrationpatterns.com/patterns/messaging/CommandMessage.html;

-spec command(Name::term(), Args::body()) ->
                     motion().
command(Name, Args) ->
    Header = #{},
    command(Header, Name, Args).

-spec command(Header::header(), Name::term(), Args::body()) ->
                     motion().
command(Header, Name, Args) ->
    erlmachine_transmission:motion(Header#{ name => Name }, Args).

%%% Document message
%% https://www.enterpriseintegrationpatterns.com/patterns/messaging/DocumentMessage.html;

-spec document(Meta::term(), Body::body()) -> 
                      motion().
document(Meta, Body) ->
    Header = #{},
    document(Header, Meta, Body).

-spec document(Header::header(), Meta::term(), Body::body()) -> 
                      motion().
document(Header, Meta, Body) ->
    erlmachine_transmission:motion(Header#{ meta => Meta }, Body).

%%% Event message
%% https://www.enterpriseintegrationpatterns.com/patterns/messaging/EventMessage.html;

-spec event(Type::term(), Desc::body()) -> 
                   motion().
event(Type, Desc) ->
    Header = #{},
    event(Header, Type, Desc).

-spec event(Header::header(), Type::term(), Desc::body()) -> 
                   motion().
event(Header, Type, Desc) ->
    erlmachine_transmission:motion(Header#{ type => Type }, Desc).

-spec command_name(Motion::motion()) -> term().
command_name(Motion) ->
    Header = header(Motion),
    maps:get(name, Header).

-spec document_meta(Motion::motion()) -> term().
document_meta(Motion) ->
    Header = header(Motion),
    maps:get(meta, Header).

-spec event_type(Motion::motion()) -> term().
event_type(Motion) ->
    Header = header(Motion),
    maps:get(type, Header).

%%% Request-Reply
%% https://www.enterpriseintegrationpatterns.com/patterns/messaging/RequestReply.html;

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
    maps:get(address, Header, undefined).

%%% Return address
%% https://www.enterpriseintegrationpatterns.com/patterns/messaging/ReturnAddress.html

-spec return_address(Motion::motion(), Address::term()) -> motion().
return_address(Motion, Address) ->
    Header = header(Motion),
    header(Motion, Header#{ address => Address }).

-spec reply(Req::motion(), Res::motion()) -> motion().
reply(Req, Res) ->
    Header = maps:merge(header(Res), header(Req)),
    header(Res, Header).

%%% Correlation identifier
%% https://www.enterpriseintegrationpatterns.com/patterns/messaging/CorrelationIdentifier.html;

-spec correlation_id(Motion::motion()) -> term().
correlation_id(Motion) ->
    Header = header(Motion),
    maps:get(id, Header, undefined).

-spec correlation_id(Motion::motion(), Id::term()) -> motion().
correlation_id(Motion, Id) ->
    Header = header(Motion),
    header(Motion, Header#{ id => Id }).

%%% Message history
%% https://www.enterpriseintegrationpatterns.com/patterns/messaging/MessageHistory.html;

-spec history(Motion::motion()) -> [term()].
history(Motion) ->
    Header = header(Motion),
    maps:get(history, Header, []).

-spec history(Motion::motion(), Log::term()) -> motion().
history(Motion, Log) ->
    Header = header(Motion),
    History = history(Motion),
    header(Motion, Header#{ history => [Log|History] }).

%%% Response API

-spec failure() -> failure().
failure() ->
    erlbox:failure().

-spec failure(E::term()) -> failure(term()).
failure(E) ->
    erlbox:failure(E).

-spec failure(E::term(), R::term()) -> failure(term(), term()).
failure(E, R) ->
    erlbox:failure(E, R).

-spec failure(E::term(), R::term(), S::term()) -> failure(term(), term(), term()).
failure(E, R, S) ->
    erlbox:failure(E, R, S).

-spec success() -> success().
success() ->
    erlbox:success().

-spec success(Res::term()) -> success(term()).
success(Res) ->
    erlbox:success(Res).

-spec success(Res::term(), S::term()) -> success(term(), term()).
success(Res, S) ->
    erlbox:success(Res, S).

-spec success(Res::term(), S::term(), A::[term()]) -> success(term(), term(), [term()]).
success(Res, S, A) ->
    erlbox:success(Res, S, A).

-spec is_success(Res::term()) -> boolean().
is_success(Res) ->
    erlbox:is_success(Res).

-spec is_failure(Res::term()) -> boolean().
is_failure(Res) ->
    erlbox:is_failure(Res).

%%% Module API

-spec attributes(Module::module()) -> [{atom(), term()}].
attributes(Module) ->
    Module:module_info(attributes).

-spec behaviours(Module::module()) -> [atom()].
behaviours(Module) ->
    [Name|| {behaviour, [Name]} <- attributes(Module)].

-spec optional_callback(Module::module(), Fun::atom(), Args::list()) -> 
                               term().
optional_callback(Module, Fun, Args) ->
    optional_callback(Module, Fun, Args, success()).

-spec optional_callback(Module::module(), Fun::atom(), Args::[term()], Def::term()) -> 
                               term().
optional_callback(Module, Fun, Args, Def) ->
    case erlang:function_exported(Module, Fun, length(Args)) of 
        true ->
            erlang:apply(Module, Fun, Args); 
        _  -> 
            Def 
    end.

-spec vsn(Module::module()) -> binary() | integer().
vsn(Module) ->
    {_, [Vsn]} = lists:keyfind('vsn', 1, attributes(Module)),
    Vsn.

%%% Assembly API

-spec serial_no(Assembly::assembly()) -> serial_no().
serial_no(Assembly) ->
    erlmachine_assembly:serial_no(Assembly).

-spec model_no(Assembly::assembly()) -> model_no().
model_no(Assembly) ->
    erlmachine_assembly:model_no(Assembly).

-spec port(Assembly::assembly()) -> term().
port(Assembly) ->
    erlmachine_assembly:port(Assembly).

-spec port(Assembly::assembly(), Port::term()) -> assembly().
port(Assembly, Port) ->
    erlmachine_assembly:port(Assembly, Port).

-spec graph(Assembly::assembly()) -> graph().
graph(Assembly) ->
    erlmachine_assembly:graph(Assembly).

-spec uid(Assembly::assembly()) -> uid().
uid(Assembly) ->
    erlmachine_assembly:uid(Assembly).

-spec tags(Assembly::assembly()) -> [term()].
tags(Assembly) ->
    erlmachine_assembly:tags(Assembly).

-spec tags(Assembly::assembly(), Tags::[term()]) -> assembly().
tags(Assembly, Tags) ->
    erlmachine_assembly:tags(Assembly, Tags).

-spec vertex(Assembly::assembly()) -> term().
vertex(Assembly) ->
    erlmachine_assembly:vertex(Assembly).

-spec vertex(Assembly::assembly(), Vertex::term()) -> assembly().
vertex(Assembly, Vertex) ->
    erlmachine_assembly:vertex(Assembly, Vertex).

-spec part_no(Assembly::assembly()) -> part_no().
part_no(Assembly) ->
    erlmachine_assembly:part_no(Assembly).

-spec description(Assembly::assembly()) -> binary().
description(Assembly) ->
    erlmachine_assembly:description(Assembly).

%%% Extensions

-spec is_supervisor(Assembly::assembly()) -> boolean().
is_supervisor(Assembly) ->
    Type = erlmachine_assembly:type(Assembly),
    Type == 'supervisor'.

-spec is_worker(Assembly::assembly()) -> boolean().
is_worker(Assembly) ->
    Type = erlmachine_assembly:type(Assembly),
    Type == 'worker'.

%% NOTE: The next format is safer and more applicable by web applications (in comparison to base64);

-spec guid(Serial::term()) -> binary().
guid(Serial) ->
    GUID = #guid{ node = node(),
                  serial = Serial,

                  reference = make_ref()
                },

    Res = md5(GUID),
    Res.

-spec md5(Data::term()) -> binary().
md5(Data) ->
    MD5 = erlang:md5(term_to_binary(Data)),
    MD5.

-spec phash2(Term::term()) -> non_neg_integer().
phash2(Term) ->
    erlang:phash2(Term, 4294967296).

%% TODO Replace with https://github.com/okeuday/uuid
%%

-spec base64url(N::binary()) -> Base64::binary().
base64url(N) when is_binary(N) ->
    Base64 = base64:encode(N),

    Base64Url = [ begin case Char of
                            $+ -> <<"-">>;
                            $/ -> <<"_">>;
                            _ -> <<Char>>
                        end
                  end || <<Char>> <= Base64, Char /= $= ],

    Res = << <<X/binary>> || X <- Base64Url >>,
    Res.

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

-module(erlmachine).
%% NOTE: There are two transmission API: Schema based (erlmachine) and assembly based (erlmachine_transmission);
-folder(<<"erlmachine">>).

-export([start/0, stop/0, priv_dir/0]).

-export([is_supervisor/1, is_worker/1]).

-export([boot/1, boot/2]).

-export([process/3]).
-export([execute/3]).
-export([install/2, install/3]).
-export([uninstall/2, uninstall/3]).

-export([shutdown/1, shutdown/2, shutdown/3]).

-export([motion/2]).
-export([header/1, header/2, body/1, body/2]).

-export([command/3, document/3, event/3]).
-export([command_name/1, document_meta/1, event_type/1]).

-export([return_address/1, return_address/2]).
-export([correlation_id/1, correlation_id/2]).

-export([request_reply/2, request_reply/3]).

-export([failure/0, failure/1, failure/2, failure/3]).
-export([success/0, success/1, success/2]).
-export([is_success/1, is_failure/1]).

-export([attribute/3]).
-export([optional_callback/3, optional_callback/4]).

-export([serial_no/1]).
-export([schema/1]).
-export([label/1, label/2]).
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

-type schema() :: erlmachine_schema:schema().
-type vertex() :: erlmachine_schema:vertex().

-type motion() :: erlmachine_transmission:motion().
-type header() :: erlmachine_transmission:header().
-type body() :: erlmachine_transmission:body().

-record(guid, { node::node(), reference::reference(), serial::term() }).

-type guid()::#guid{}.

-export_type([guid/0]).

-spec start() -> success().
start() ->
    {ok, _} = application:ensure_all_started(?MODULE),
    success().

-spec stop() -> success() | failure(Reason :: any()).
stop() ->
    application:stop(?MODULE).

-spec priv_dir() -> file:filename().
priv_dir() ->
    code:priv_dir(?MODULE).

-spec is_supervisor(Assembly::assembly()) -> boolean().
is_supervisor(Assembly) ->
    Name = erlmachine_assembly:name(Assembly),
    Name:type() == 'supervisor'.

-spec is_worker(Assembly::assembly()) -> boolean().
is_worker(Assembly) ->
    Name = erlmachine_assembly:name(Assembly),
    Name:type() == 'worker'.

%% The main purpouse of erlmachine project is to provide a set of well desikgned behaviours which are accompanied with visualization tools as well.
%%  Erlmachine doesn't restrict your design with the one possible way but instead provide you ability to implement your own components accordingly to your vison.
%% This ability is available under flexible mechanism of prototypes and overloading (models).

-spec boot(Schema::schema()) ->
                   success(pid()) | ingnore | failure(term()).
boot(Schema) ->
    Root = erlmachine_schema:root(Schema),
    boot(Schema, Root).

-spec boot(Schema::assembly(), V::vertex()) ->
                   success(pid()) | ingnore | failure(term()).
boot(Schema, V) ->
    erlmachine_transmission:boot(Schema, V).

-spec process(Schema::schema(), V::vertex(), Motion::term()) ->
                    term().
process(Schema, V, Motion) ->
    erlmachine_transmission:process(Schema, V, Motion).

-spec execute(Schema::assembly(), V::vertex(), Command::term()) ->
                      term().
execute(Schema, V, Command) ->
    erlmachine_transmission:execute(Schema, V, Command).

-spec install(Schema::schema(), Ext::assembly()) ->
                     success(pid()) | ingnore | failure(term()).
install(Schema, Ext) ->
    Root = erlmachine_schema:root(Schema),
    install(Schema, Root, Ext).

-spec install(Schema::schema(), V::vertex(), Ext::assembly()) ->
                     success(pid()) | ingnore | failure(term()).
install(Schema, V, Ext) ->
    erlmachine_transmission:install(Schema, V, Ext).

-spec uninstall(Schema::schema(), Id::term()) ->
                       success().
uninstall(Schema, Id) ->
    Root = erlmachine_schema:root(Schema),
    erlmachine_transmission:uninstall(Schema, Root, Id).

-spec uninstall(Schema::schema(), V::vertex(), Id::term()) ->
                       success().
uninstall(Schema, V, Id) ->
    erlmachine_transmission:uninstall(Schema, V, Id).

-spec shutdown(Schema::schema()) ->
                      success().
shutdown(Schema) ->
    Root = erlmachine_schema:root(Schema),
    shutdown(Schema, Root).

-spec shutdown(Schema::schema(), V::vertex()) ->
                  success().
shutdown(Schema, V) ->
    Reason = normal,
    shutdown(Schema, V, Reason).

-spec shutdown(Schema::schema(), V::vertex(), Reason::term()) ->
                      success().
shutdown(Schema, V, Reason) ->
    Timeout = 5000,
    erlmachine_transmission:shutdown(Schema, V, Reason, Timeout).

%%%===================================================================
%%%  Message construction API
%%%===================================================================

-spec motion(Header::header(), Body::body()) -> motion().
motion(Header, Body) ->
    erlmachine_transmission:motion(Header, Body).

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

-spec command(Header::header(), Name::term(), Args::body()) ->
                     motion().
command(Header, Name, Args) ->
    erlmachine_transmission:motion(Header#{ command => Name }, Args).

-spec document(Header::header(), Meta::term(), Body::body()) -> 
                      motion().
document(Header, Meta, Body) ->
    erlmachine_transmission:motion(Header#{ document => Meta }, Body).

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

-spec request_reply(Motion::motion(), Address::term()) -> 
                           motion().
request_reply(Motion, Address) ->
    return_address(Motion, Address).

-spec request_reply(Motion::motion(), Address::term(), Id::term()) -> 
                           motion().
request_reply(Motion, Address, Id) ->
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

%%%===================================================================
%%% Reply API
%%%===================================================================

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

-spec attribute(Module::atom(), Tag::atom(), Default::term()) -> false | {Tag::atom(), Value::term()}.
attribute(Module, Tag, Default) ->
    Attributes = Module:module_info(attributes),
    Result = lists:keyfind(Tag, 1, Attributes),
    case Result of 
        false -> 
            Default;
        {Tag, Data} -> 
            Data 
    end.

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

-spec serial_no(Assembly::assembly()) -> serial_no().
serial_no(Assembly) ->
    erlmachine_assembly:serial_no(Assembly).

-spec schema(Assembly::assembly()) -> term().
schema(Assembly) ->
    erlmachine_assembly:schema(Assembly).

-spec label(Assembly::assembly()) -> term().
label(Assembly) ->
    erlmachine_assembly:label(Assembly).

-spec label(Assembly::assembly(), Label::term()) -> assembly().
label(Assembly, Label) ->
    erlmachine_assembly:label(Assembly, Label).

-spec tags(Assembly::assembly()) -> list(). 
tags(Assembly) ->
    erlmachine_assembly:tags(Assembly).

-spec part_no(Assembly::assembly()) -> part_no().
part_no(Assembly) ->
    erlmachine_assembly:part_no(Assembly).

-spec description(Assembly::assembly()) -> binary().
description(Assembly) ->
    erlmachine_assembly:description(Assembly).

%% NOTE: To produce a readable string representation of a SN/MN/PN/TN;
%%
%% base64url encoding was provided; 
%% This format is safer and more applicable by web applications (in comparison with base64);

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

-spec timestamp() -> integer().
timestamp() ->
    erlang:system_time(seconds).

-spec tag(Assembly::assembly(), Tag::term()) -> assembly().
tag(Assembly, Tag) ->
    Tags = erlmachine_assembly:tags(Assembly),
    erlmachine_assembly:tags(Assembly, [Tag|Tags]).

-spec untag(Assembly::assembly(), Tag::term()) -> assembly().
untag(Assembly, Tag) ->
    Tags = erlmachine_assembly:tags(Assembly),
    erlmachine_assembly:tags(Assembly, lists:delete(Tag, Tags)).

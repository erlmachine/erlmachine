-module(erlmachine_scope).

-behaviour(syn_event_handler).

-export([is_scope/1, scopes/1]).

-export([init/1]).

-export([join/3, join/4]).
-export([leave/3]).

-export([publish/3]).
-export([call/3, call/4]).

-export([is_member/3, members/2]).

-export([on_process_joined/5, on_process_left/5]).

-include_lib("erlbox/include/erlbox.hrl").

-include("erlmachine_assembly.hrl").
-include("erlmachine_factory.hrl").

-callback scope() -> module().

-callback process_joined(Group::term(), Pid::pid(), Meta::term()) ->
    success().

-callback process_left(Group::term(), Pid::pid(), Meta::term(), Reason::atom()) ->
    success().

-optional_callbacks([process_joined/3, process_left/4]).

-type member() :: {pid(), term()}.

%%% Modules

-spec is_scope(Module::module()) -> boolean().
is_scope(Module) ->
    lists:member(?MODULE, erlmachine:behaviours(Module)).

-spec scopes(Modules::[module()]) -> [module()].
scopes(Modules) ->
    [M || M <- Modules, is_scope(M)].

%%% Init

-spec init(Scopes::[module()]) -> success().
init(Scopes) ->
    ok = syn:add_node_to_scopes(Scopes),

    syn:set_event_handler(?MODULE).

%%% Group

-spec join(Module::module(), Group::term(), Pid::pid()) ->
                  success().
join(Module, Group, Pid) ->
    join(Module, Group, Pid, _Meta = undefined).

-spec join(Module::module(), Group::term(), Pid::pid(), Meta::term()) ->
                  success().
join(Module, Group, Pid, Meta) ->
    Scope = Module:scope(),

    ok = syn:join(Scope, Group, Pid, Meta).

-spec leave(Module::module(), Group::term(), Pid::pid()) ->
                   success().
leave(Module, Group, Pid) ->
    Scope = Module:scope(),

    ok = syn:leave(Scope, Group, Pid).

-spec publish(Module::module(), Group::term(), Message::term()) ->
                     success(integer()).
publish(Module, Group, Message) ->
    Scope = Module:scope(),

    {ok, _Count} = syn:publish(Scope, Group, Message).

-spec call(Module::module(), Group::term(), Message::term()) ->
                  {[{member(), term()}], [member()]}.
call(Module, Group, Message) ->
    call(Module, Group, Message, 5000).

-spec call(Module::module(), Group::term(), Message::term(), Timeout::integer()) ->
                  {[{member(), term()}], [member()]}.
call(Module, Group, Message, Timeout) ->
    Scope = Module:scope(),

    syn:multi_call(Scope, Group, Message, Timeout).

-spec is_member(Module::module(), Group::term(), Pid::pid()) -> boolean().
is_member(Module, Group, Pid) ->
    Scope = Module:scope(),

    syn:is_member(Scope, Group, Pid).

-spec members(Module::module(), Group::term()) -> [{pid(), term()}].
members(Module, Group) ->
    Scope = Module:scope(),

    syn:members(Scope, Group).

-spec on_process_joined(Module::module(), Group::term(), Pid::pid(), Meta::term(), Reason::atom()) ->
                               success().
on_process_joined(Module, Group, Pid, Meta, _Reason) ->
    erlmachine:optional_callback(Module, 'process_joined', [Group, Pid, Meta]).

-spec on_process_left(Module::module(), Group::term(), Pid::pid(), Meta::term(), Reason::atom()) ->
                             success().
on_process_left(Module, Group, Pid, Meta, Reason) ->
    erlmachine:optional_callback(Module, 'process_left', [Group, Pid, Meta, Reason]).

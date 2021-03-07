-module(erlmachine_registry).

-export([is_registry/1]).

-export([join/2, join/3]).
-export([publish/2]).
-export([get_members/1, member/2]).

-export([register/2, register/3]).
-export([whereis/1]).

-include("erlmachine_assembly.hrl").
-include("erlmachine_factory.hrl").
-include("erlmachine_system.hrl").

-callback group() -> term().

%%% Modules

-spec is_registry(Module::atom()) -> boolean().
is_registry(Module) ->
    lists:member(?MODULE, erlmachine:behaviours(Module)).

%%% Groups

-spec join(Module::atom(), Pid::pid()) -> success().
join(Module, Pid) ->
    join(Module, Pid, _Meta = undefined).

-spec join(Module::module(), Pid::pid(), Meta::term()) -> success().
join(Module, Pid, Meta) ->
    Group = Module:group(),
    ok = syn:join(Group, Pid, Meta).

-spec publish(Module::atom(), Message::term()) -> success(integer()).
publish(Module, Message) ->
    Group = Module:group(),
    {ok, _Count} = syn:publish(Group, Message).

-spec get_members(Module::atom()) -> [{pid(), term()}].
get_members(Module) ->
    Group = Module:group(),
    syn:get_members(Group, with_meta).

-spec member(Pid::pid(), Module::atom()) -> boolean().
member(Pid, Module) ->
    Group = Module:group(),
    syn:member(Pid, Group).

%%% Processes

-spec register(Name::term(), Pid::pid()) -> success().
register(Name, Pid) ->
    register(Name, Pid, _Meta = undefined).

-spec register(Name::term(), Pid::pid(), Meta::term()) -> success().
register(Name, Pid, Meta) ->
    ok = syn:register(Name, Pid, Meta).

-spec whereis(Name::term()) -> success(pid(), term()) | undefined.
whereis(Name) ->
    syn:whereis(Name, with_meta).


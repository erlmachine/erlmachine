-module(erlmachine_supervisor_model).
%% NOTE: The main purpouse of the supervisor model is the ability to make impact on runtime layer without affecting of monitoring layer of service;

%% NOTE: Supervisor model concerns: runtime credentials, logging;

%% NOTE: UID is used to support runtime credentials of the initiator of supervisor process:
%%  - https://en.wikipedia.org/wiki/Group_identifier;
%%  - https://en.wikipedia.org/wiki/User_identifier;

%% NOTE: Permissions are set of characters which represents the read, write, and execute access;
%% TODO: Permissions should be checked automatically before invocation;

%% TODO: https://www.kernel.org/doc/html/latest/security/credentials.html;

%% TODO:
%%   1. https://wiki.archlinux.org/index.php/File_permissions_and_attributes;
%%   2. https://en.wikipedia.org/wiki/File-system_permissions;
%%   3. https://en.wikipedia.org/wiki/File_attribute;
%%   4. https://mason.gmu.edu/~montecin/UNIXpermiss.htm;
%%   5. https://www.howtogeek.com/437958/how-to-use-the-chmod-command-on-linux/

%% API

-export([is_supervisor_model/1]).

-export([startup/2]).
-export([install/2, uninstall/2]).

-include("erlmachine_user.hrl").
-include("erlmachine_factory.hrl").
-include("erlmachine_assembly.hrl").
-include("erlmachine_system.hrl").
-include("erlmachine_transmission.hrl").


-callback startup(UID::uid(), Vertices::[vertex()], Opt::[term()], Env::map()) ->
    success() | failure(term(), term()).

-callback install(UID::uid(), Vertex::vertex()) ->
    success() | failure(term(), term()).

-callback uninstall(UID::uid(), Vertex::vertex()) ->
    success() | failure(term(), term()).

-optional_callbacks([install/2, uninstall/2]).

-spec is_supervisor_model(Module::atom()) -> boolean().
is_supervisor_model(Module) ->
    lists:member(?MODULE, erlmachine:behaviours(Module)).

%%%  Transmission API

-spec startup(Context::assembly(), Vs::[vertex()]) ->
                  success() | failure(term(), term()).
startup(Context, Vs) ->
    Model = erlmachine_assembly:model(Context), Name = erlmachine_model:name(Model),
    UID = erlmachine_assembly:uid(Context),

    Opt = erlmachine_model:options(Model),
    Env = erlmachine_assembly:env(Context),

    Name:startup(UID, Vs, Opt, Env).

-spec install(Context::assembly(), Vertex::vertex()) ->
                     success() | failure(term(), term()).
install(Context, Vertex) ->
    Model = erlmachine_assembly:model(Context), Name = erlmachine_model:name(Model),
    UID = erlmachine_assembly:uid(Context),

    Mod = Name, Fun = install, Args = [UID, Vertex],
    Def = erlmachine:success(),
    erlmachine:optional_callback(Mod, Fun, Args, Def).

-spec uninstall(Context::assembly(), Vertex::vertex()) ->
                       success() | failure(term(), term()).
uninstall(Context, Vertex) ->
    Model = erlmachine_assembly:model(Context), Name = erlmachine_model:name(Model),
    UID = erlmachine_assembly:uid(Context),

    Mod = Name, Fun = uninstall, Args = [UID, Vertex],
    Def = erlmachine:success(),
    erlmachine:optional_callback(Mod, Fun, Args, Def).

-module(erlmachine_supervisor).
%% NOTE: The main purpouse of the supervisor model is the ability to make impact on runtime layer without affecting monitoring layer of service;

%% NOTE: Supervisor model concerns: runtime credentials;

%% NOTE: UID and GID are credentials of the initiator of supervisor process:
%%  - https://en.wikipedia.org/wiki/Group_identifier;
%%  - https://en.wikipedia.org/wiki/User_identifier;

%% NOTE: Permissions are a set of characters which represents the read, write, and execute access;
%% TODO: Permissions should be checked automatically before invocation;

%% TODO: https://www.kernel.org/doc/html/latest/security/credentials.html;

%% TODO:
%%   1. https://wiki.archlinux.org/index.php/File_permissions_and_attributes;
%%   2. https://en.wikipedia.org/wiki/File-system_permissions;
%%   3. https://en.wikipedia.org/wiki/File_attribute;
%%   4. https://mason.gmu.edu/~montecin/UNIXpermiss.htm;
%%   5. https://www.howtogeek.com/437958/how-to-use-the-chmod-command-on-linux/

%% API
-export([boot/2]).

-export([install/2]).
-export([uninstall/2]).

-export([shutdown/1]).

-type spec() :: map().

-include("erlmachine_user.hrl").
-include("erlmachine_factory.hrl").
-include("erlmachine_assembly.hrl").
-include("erlmachine_system.hrl").

-callback boot(MN::model_no(), UID::uid(), GID::gid(), Specs::[spec()], Opt::list(), Env::list()) -> 
    success() | failure(term(), term()).

-callback install(MN::model_no(), UID::uid(), GID::gid(), Spec::spec()) -> 
    success() | failure(term(), term()).

-callback uninstall(MN::model_no(), UID::uid(), GID::gid(), ID::term()) ->
    success() | failure(term(), term()).

-callback shutdown(MN::model_no(), UID::uid(), GID::gid()) ->
    success() | failure(term(), term()).

-optional_callbacks([install/4, uninstall/4, shutdown/3]).

-spec boot(Context::assembly(), Specs::[spec()]) ->
                  success() | failure(term(), term()).
boot(Context, Specs) ->
    MN = erlmachine_assembly:model_no(Context),

    Model = erlmachine_assembly:model(Context), Name = erlmachine_model:name(Model),
    UID = erlmachine_assembly:uid(Context), GID = erlmachine_assembly:gid(Context),

    Opt = erlmachine_model:options(Model),
    Env = erlmachine_assembly:env(Context),

    Name:boot(MN, UID, GID, Specs, Opt, Env).

-spec install(Context::assembly(), Spec::spec()) ->
                     success() | failure(term(), term()).
install(Context, Spec) ->
    MN = erlmachine_assembly:model_no(Context),

    Model = erlmachine_assembly:model(Context), Name = erlmachine_model:name(Model),
    UID = erlmachine_assembly:uid(Context), GID = erlmachine_assembly:gid(Context),

    Name:install(MN, UID, GID, Spec).

-spec uninstall(Context::assembly(), ID::term()) ->
                       success() | failure(term(), term()).
uninstall(Context, ID) ->
    MN = erlmachine_assembly:model_no(Context),

    Model = erlmachine_assembly:model(Context), Name = erlmachine_model:name(Model),
    UID = erlmachine_assembly:uid(Context), GID = erlmachine_assembly:gid(Context),

    Name:uninstall(MN, UID, GID, ID).

-spec shutdown(Context::assembly()) ->
                      success() | failure(term(), term()).
shutdown(Context) ->
    MN = erlmachine_assembly:model_no(Context),

    Model = erlmachine_assembly:model(Context), Name = erlmachine_model:name(Model),
    UID = erlmachine_assembly:uid(Context), GID = erlmachine_assembly:gid(Context),

    Name:shutdown(MN, UID, GID).

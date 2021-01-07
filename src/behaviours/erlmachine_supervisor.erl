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

-export([shutdown/2]).

-include("erlmachine_user.hrl").
-include("erlmachine_factory.hrl").
-include("erlmachine_assembly.hrl").
-include("erlmachine_system.hrl").
-include("erlmachine_transmission.hrl").


-callback boot(UID::uid(), Specs::[spec()], Opt::[term()], Env::map()) -> 
    success() | failure(term(), term()).

-callback install(UID::uid(), Spec::spec()) ->
    success() | failure(term(), term()).

-callback uninstall(UID::uid(), ID::term()) ->
    success() | failure(term(), term()).

-callback shutdown(UID::uid(), Reason::term()) ->
    success() | failure(term(), term()).

-optional_callbacks([install/2, uninstall/2, shutdown/2]).

-spec boot(Context::assembly(), Specs::[spec()]) ->
                  success() | failure(term(), term()).
boot(Context, Specs) ->
    Model = erlmachine_assembly:model(Context), Name = erlmachine_model:name(Model),
    UID = erlmachine_assembly:uid(Context),

    Opt = erlmachine_model:options(Model), Env = erlmachine_assembly:env(Context),

    Name:boot(UID, Specs, Opt, Env).

-spec install(Context::assembly(), Spec::spec()) ->
                     success() | failure(term(), term()).
install(Context, Spec) ->
    Model = erlmachine_assembly:model(Context), Name = erlmachine_model:name(Model),
    UID = erlmachine_assembly:uid(Context), 

    Name:install(UID, Spec).

-spec uninstall(Context::assembly(), ID::term()) ->
                       success() | failure(term(), term()).
uninstall(Context, ID) ->
    Model = erlmachine_assembly:model(Context), Name = erlmachine_model:name(Model),
    UID = erlmachine_assembly:uid(Context), 

    Name:uninstall(UID, ID).

-spec shutdown(Context::assembly(), Reason::term()) ->
                      success() | failure(term(), term()).
shutdown(Context, Reason) ->
    Model = erlmachine_assembly:model(Context), Name = erlmachine_model:name(Model),
    UID = erlmachine_assembly:uid(Context),

    Mod = Name, Fun = shutdown, Args = [UID, Reason],
    Def = erlmachine:success(),
    ok = erlmachine:optional_callback(Mod, Fun, Args, Def).

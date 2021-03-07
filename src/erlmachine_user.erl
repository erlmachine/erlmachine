-module(erlmachine_user).
%% NOTE: https://en.wikipedia.org/wiki/Superuser
%% API.
-export([root/0]).

-include("erlmachine_system.hrl").
%% NOTE: User -1 can be assigned to the automated system objects;
-record(user, {
               %% The identity of a user
               id::uid(),
               %% User name
               name::binary(),
               %% User title
               title::binary(),
               %% Short summary
               summary::binary()
              }
       ).

-opaque user() :: #user{}.
-type uid() :: integer().

-export_type([user/0, uid/0]).

-spec root() -> 0.
root() ->
    0.






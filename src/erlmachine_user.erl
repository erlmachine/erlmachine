-module(erlmachine_user).
%% NOTE: https://en.wikipedia.org/wiki/Superuser
%% API.
-export([root/0]).

-include_lib("erlbox/include/erlbox.hrl").

%% NOTE: User -1 can be assigned to mark the automated system objects;
-record(user, {
               id::uid(),

               name::binary(), title::binary(),
               summary::binary()
              }
       ).

-opaque user() :: #user{}.
-type uid() :: integer().

-export_type([user/0, uid/0]).

-spec root() -> 0.
root() ->
    0.






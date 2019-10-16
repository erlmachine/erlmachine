-module(erlmachine_filesystem).
-behaviour(gen_server).

%% API.
-export([start_link/0]).

%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-export([folder/1]).

-export([read/1, write/2]).

-include("erlmachine_system.hrl").

-type path()::binary()|atom().

-export_types([path/0]).

-spec folder(Module::atom()) -> binary().
folder(Module) ->
    Folder = erlmachine:attribute(Module, folder),
    Folder.

-spec read(Path::path()) -> success(Body::term()) | failure(E::term(), R::term()).
read(_Name) ->
    %% TODO implent this mock;
     {ok, []}.

-spec write(Name::path(), Body::term()) -> success() | failure(E::term(), R::term()).
write(_Name, _Body) ->
    %% TODO implent this mock;
    ok.

-record(state, {
}).

%% Name: Name is the symbolic file name and is the only information kept in human readable form.
%% Identifier: This unique tag is a number that identifies the file within the file system; it is in non-human-readab%% le form of the file.
%% Type: This information is needed for systems which support different types of files or its format.
%% Location: This information is a pointer to a device which points to the location of the file on the device where it is stored.
%% Size: The current size of the file (which is in bytes, words, etc.) which possibly the maximum allowed size gets included in this attribute.
%% Protection: Access-control information establishes who can do the reading, writing, executing, etc.
%% Date, Time & user identification: This information might be kept for the creation of the file, its last modification and last used. These data might be useful for in the field of protection, security, and monitoring its usage.

%% The operating system can provide system calls to create, write, read, reposition, delete, and truncate files.

%% API.

-spec start_link() -> {ok, pid()}.
start_link() ->
	gen_server:start_link(?MODULE, [], []).

%% gen_server.

init([]) ->
	{ok, #state{}}.

handle_call(_Request, _From, State) ->
	{reply, ignored, State}.

handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

-module(erlmachine).

-folder(<<"erlmachine">>).

-export([attribute/3]).

-export([guid/0, guid/1, serial/0, serial/1, read_serial/1, write_serial/2]).

-include("erlmachine_system.hrl").
-include("erlmachine_filesystem.hrl").

-type serial()::integer().

-record(guid, {node::node(), reference::reference(), serial::serial()}).

-type guid()::#guid{}.

-export_types([serial/0, guid/0]).

%% The main purpouse of erlmachine project is providing a set of well designed behaviours which are accompanied with visualization tools as well.
%%  Erlmachine doesn't restrict your workflow by the one possible way but instead provide to you ability to implement your own components. This ability is available under flexible mechanism of prototypes and overloading.  

-spec attribute(Module::atom(), Tag::atom(), Default::term()) -> false | {Tag::atom(), Value::term()}.
attribute(Module, Tag, Default) ->
    Attributes = Module:module_info(attributes),
    Result = lists:keyfind(Tag, 1, Attributes),
    case 
        Result of false -> Default;
        {Tag, Data} -> Data 
    end.

-spec guid() -> GUID::guid().
guid() ->
    guid(0).

-spec guid(Serial::serial()) -> GUID::guid().
guid(Serial) ->
    GUID = #guid{node=node(), serial=Serial, reference=make_ref()},
    MD5 = erlang:md5(term_to_binary(GUID)),
    MD5.
    

%% At this point we provide persisnence layer over serial counter;
%% Until persistence layer exists we can be sureabout uniqueness of SN;
%% When persistence layer is lost it's usually about both kind of data (seed and actually data itself);

-spec read_serial(Path::path()) -> success(Serial::integer()) | failure(E::term(), R::term()).
read_serial(Path) ->
    Serial =
        case erlmachine_filesystem:read(Path) of
           {ok, [Num]} ->
                {ok, Num};
            {ok, _} ->
                {ok, serial()};
            {error, _} = Error ->
                Error
        end,
    Serial.

%% At that place we consider to rewrite file instead of append;
-spec write_serial(Path::path(), Serial::integer()) -> success() | failure(E::term(), R::term()).
write_serial(Path, Serial) ->
    Result = erlmachine_filesystem:write(Path, [Serial]),
    Result.

-spec serial() -> serial().
serial() ->
    0.

-spec serial(Serial::serial()) -> serial().
serial(Serial) ->
    Serial + 1.


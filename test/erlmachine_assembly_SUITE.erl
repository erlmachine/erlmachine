-module(erlmachine_assembly_SUITE).

-export([suite/0]).

-export([init_per_suite/1, end_per_suite/1]).
-export([init_per_group/2, end_per_group/2]).
-export([init_per_testcase/2, end_per_testcase/2]).
-export([groups/0]).

-export([all/0]).

-export([install/0, install/1]).
-export([assemble/0, assemble/1]).
-export([disassemble/0, disassemble/1]).
-export([uninstall/0, uninstall/1]).

 -include_lib("common_test/include/ct.hrl").

%%--------------------------------------------------------------------
%% COMMON TEST CALLBACK FUNCTIONS
%%--------------------------------------------------------------------

suite() -> 
    [{timetrap,{minutes,1}}].

init_per_suite(Config) ->
    Nodes = [node()],
    mnesia:create_schema(Nodes), ok = mnesia:start(),
    {ok, _Pid} = erlmachine_factory:start(),
    Config.

end_per_suite(_Config) ->
    mnesia:stop(),
    ok = erlmachine_factory:stop().

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, _Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.

%%--------------------------------------------------------------------
%% Function: groups() -> [Group]
%%
%% Group = {GroupName,Properties,GroupsAndTestCases}
%% GroupName = atom()
%%   The name of the group.
%% Properties = [parallel | sequence | Shuffle | {RepeatType,N}]
%%   Group properties that may be combined.
%% GroupsAndTestCases = [Group | {group,GroupName} | TestCase]
%% TestCase = atom()
%%   The name of a test case.
%% Shuffle = shuffle | {shuffle,Seed}
%%   To get cases executed in random order.
%% Seed = {integer(),integer(),integer()}
%% RepeatType = repeat | repeat_until_all_ok | repeat_until_all_fail |
%%              repeat_until_any_ok | repeat_until_any_fail
%%   To get execution of cases repeated.
%% N = integer() | forever
%%
%% Description: Returns a list of test case group definitions.
%%--------------------------------------------------------------------
groups() ->
    [{gearbox, [sequence], [install, assemble, disassemble, uninstall]}].

%%--------------------------------------------------------------------
%% Function: all() -> GroupsAndTestCases | {skip,Reason}
%%
%% GroupsAndTestCases = [{group,GroupName} | TestCase]
%% GroupName = atom()
%%   Name of a test case group.
%% TestCase = atom()
%%   Name of a test case.
%% Reason = term()
%%   The reason for skipping all groups and test cases.
%%
%% Description: Returns the list of groups and test cases that
%%              are to be executed.
%%--------------------------------------------------------------------
all() ->
    [{group, gearbox}].

%%--------------------------------------------------------------------
%% TEST CASES
%%--------------------------------------------------------------------

install() ->
    [].

%%--------------------------------------------------------------------
%% Function: TestCase(Config0) ->
%%               ok | exit() | {skip,Reason} | {comment,Comment} |
%%               {save_config,Config1} | {skip_and_save,Reason,Config1}
%%
%% Config0 = Config1 = [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%% Reason = term()
%%   The reason for skipping the test case.
%% Comment = term()
%%   A comment about the test case that will be printed in the html log.
%%
%% Description: Test case function. (The name of it must be specified in
%%              the all/0 list or in a test case group for the test case
%%              to be executed).
%%--------------------------------------------------------------------
install(Config) ->
    %% TODO: TO make isntallation via allocated driver process (erlmachine_sample);
    GearBox = erlmachine_sample:gearbox(_Opt = [], _Env = #{}, _Exts = []),
    {ok, Pid} = erlmachine_assembly:install(GearBox), true = is_pid(Pid),
    {comment, Pid}.

assemble() ->
    [].

assemble(_Config) ->
    ok.


disassemble() ->
    [].

disassemble(_Config) ->
    ok.

uninstall() ->
    [].

uninstall(_Config) ->
    ok.

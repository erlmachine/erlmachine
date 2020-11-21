-module(erlmachine_transmission_SUITE).

-export([suite/0]).

-export([init_per_suite/1, end_per_suite/1]).
-export([init_per_group/2, end_per_group/2]).
-export([init_per_testcase/2, end_per_testcase/2]).
-export([groups/0]).

-export([all/0]).

-export([install/0, install/1]).
-export([uninstall/0, uninstall/1]).
-export([stop/0, stop/1]).

 -include_lib("common_test/include/ct.hrl").

%%--------------------------------------------------------------------
%% COMMON TEST CALLBACK FUNCTIONS
%%--------------------------------------------------------------------

suite() -> 
    [{timetrap,{minutes,1}}].

init_per_suite(Config) ->
    Nodes = [node()],
    mnesia:create_schema(Nodes), ok = mnesia:start(),
    ok = mnesia:wait_for_tables([erlmachine_factory:tabname()], 1000),
    {ok, _} = erlmachine_factory:start(),

    Gear = erlmachine_sample:gear([]),
    Opt = [],
    Env = #{},
    Exts = [Gear],
    GearBox = erlmachine_sample:gearbox(Opt, Env, Exts),
    {ok, Pid} = erlmachine_sample:start(GearBox),

    Setup = [{pid, Pid}],
    lists:concat([Setup, Config]).

end_per_suite(Config) ->
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
    [{sample, [sequence], [install, uninstall, stop]}].

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
    [{group, sample}].

%%--------------------------------------------------------------------
%% TEST CASES
%%--------------------------------------------------------------------

start() ->
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
install() ->
    [].

install(Config) ->
    SN = ?config(serial_no, Config),
    Ext = ?config(extension, Config),
    erlmachine_sample:install(SN, 'root', Ext),
    ok.

uninstall() ->
    [].

uninstall(_Config) ->
    ok.

stop() ->
    [].

stop(Config) ->
    Pid = ?config(pid, Config),
    ok = erlmachine_sample:stop(Pid).

-module(erlmachine_SUITE).

-export([suite/0]).

-export([init_per_suite/1, end_per_suite/1]).
-export([init_per_group/2, end_per_group/2]).
-export([init_per_testcase/2, end_per_testcase/2]).
-export([groups/0]).

-export([all/0]).

-export([process/1, execute/1, pressure/1]).
-export([install/1, uninstall/1]).
-export([shutdown/1]).

 -include_lib("common_test/include/ct.hrl").

%%--------------------------------------------------------------------
%% COMMON TEST CALLBACK FUNCTIONS
%%--------------------------------------------------------------------

suite() ->
    [{timetrap,{minutes,1}}].

init_per_suite(Config) ->
    meck:expect(erlmachine, modules, 0, ['erlmachine_assembly']),

    application:start(yamerl), application:start(syn),

    Nodes = [node()], Table = 'erlmachine_factory',

    erlmachine_db:create_schema(Nodes), ok = erlmachine_db:start(),

    erlmachine_db:create_table(Table), ok = erlmachine_db:wait_for_tables([Table], 1000),

    {ok, _} = erlmachine_factory:start(),

    Tags = ['ct', 'test'],

    Ext = erlmachine:tags(erlmachine_factory:gear('erlmachine_model_ct', _Opt = #{}, _Env = #{}), Tags),
    Ext2 = erlmachine:vertex(Ext, 'test'),

    Root = erlmachine:tags(erlmachine_factory:gearbox('erlmachine_sup_model_ct', _Opt = #{}, _Env = #{}, [Ext2]), Tags),

    {ok, Pid} = erlmachine_ct:start(Root), true = is_pid(Pid),
    Setup = [], %%TODO: To provide test case args;
    lists:concat([Setup, Config]).

end_per_suite(Config) ->
    ok.

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
    [{sample, [sequence], [install, process, execute, pressure, uninstall, shutdown]}].

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
install(_Config) ->
    Tags = ['ct', 'test2', 'install'],

    Ext = erlmachine:tags(erlmachine_factory:gear(erlmachine_model_ct, _Opt = #{}, _Env = #{}), Tags),
    Ext2 = erlmachine:vertex(Ext, 'test2'),

    {ok, Pid} = erlmachine_ct:install(Ext2), true = is_pid(Pid),
    {comment, Pid}.

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

process(_Config) ->
    Event = erlmachine:event(#{}, 'ping', <<"ping">>),
    ok = erlmachine_ct:add_edge('test', 'test2'), ok = erlmachine_ct:process('test', Event).

execute(_Config) ->
    Com = erlmachine:command(#{}, 'test', []),
    ok = erlmachine_ct:execute('test', Com).

pressure(_Config) ->
    ok = erlmachine_ct:pressure('test', <<"load">>).

uninstall(_Config) ->
    ok = erlmachine_ct:uninstall('test2').

shutdown(_Config) ->
    ok = erlmachine_ct:shutdown('test').

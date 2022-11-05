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
    Nodes = [node()], mnesia:create_schema(Nodes),

    ok = mnesia:start(),
    ok = application:start(yamerl),
    ok = application:start(syn),

    Modules = [ 'erlmachine_factory', 'erlmachine_assembly', 'erlmachine_graph',
                'erlmachine_system'
              ],

    meck:expect(erlmachine, modules, 0, Modules),

    ok = erlmachine:init(Modules),

    ok = mnesia:wait_for_tables(['erlmachine_factory'], 1000),

    {ok, _} = erlmachine_factory:start(),

    Tags = ['ct', 'test'],

    Model = 'erlmachine_model_ct',
    Prot = 'erlmachine_prototype_dbg',

    Env = #{},

    Ext = erlmachine_factory:gear(Model, #{}, Prot, #{},  Env),
    Ext2 = erlmachine:vertex(Ext, 'test'),
    Ext3 = erlmachine:tags(Ext2, Tags),

    Assembly = erlmachine_factory:gearbox('erlmachine_sup_model_ct', #{}, Env, [Ext3]),
    Assembly2 = erlmachine:tags(Assembly, Tags),

    {ok, Pid} = erlmachine_ct:start(Assembly2), true = is_pid(Pid),

    %%TODO: Test case args;
    Setup = lists:append(_Setup = [], Config),
    Setup.

end_per_suite(Config) ->
    ok = erlmachine_factory:stop(),

    ok = application:stop(yamerl),
    ok = application:stop(syn),

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

install(_Config) ->
    Tags = ['ct', 'test2', 'install'],

    Model = erlmachine_model_ct,
    Env = #{},

    Ext = erlmachine:tags(erlmachine_factory:gear(Model, #{}, Env), Tags),
    Ext2 = erlmachine:vertex(Ext, 'test2'),

    {ok, Pid} = erlmachine_ct:install(Ext2), true = is_pid(Pid),
    {comment, Pid}.

process(_Config) ->
    Event = erlmachine:event(#{}, 'ping', <<"ping">>),

    Res = erlmachine_ct:add_edge('test', 'test2'),
    Res = erlmachine_ct:process('test', Event),

    Res = ok.

execute(_Config) ->
    Com = erlmachine:command(#{}, 'test', []),
    Res = erlmachine_ct:execute('test', Com),

    Res = ok.

pressure(_Config) ->
    Res = erlmachine_ct:pressure('test', <<"load">>),
    Res = ok.

uninstall(_Config) ->
    Res = erlmachine_ct:uninstall('test2'),
    Res = ok.

shutdown(_Config) ->
    Res = erlmachine_ct:shutdown('test'),
    Res = ok.

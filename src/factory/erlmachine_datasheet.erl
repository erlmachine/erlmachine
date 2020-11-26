-module(erlmachine_datasheet).
%% We assume that this module can provide additional set of tools:
%% 1) Navigation over document;
%% 2) Validate document structure;

%% NOTE: The module is validated via https://json-schema.org

%% API.
-export([datasheet/1]).
-export([is_datasheet/1]).

-export([gear/1, gear/2]).
-export([shaft/1, shaft/2]).
-export([axle/1, axle/2]).
-export([gearbox/2, gearbox/3]).

-type datasheet() :: list().
-type path() :: list().

-export_type([datasheet/0]).

-include("erlmachine_factory.hrl").
-include("erlmachine_assembly.hrl").
-include("erlmachine_system.hrl").

%% TODO: To make validation via https://json-schema.org;
-spec is_datasheet(Term::term()) -> boolean().
is_datasheet(Term) ->
    %% TODO: To provide semantics check;
    true = is_list(Term).

-spec datasheet(Path::path()) ->
                  success(datasheet()) | failure(term(), term()).
datasheet(Path) ->
    try
        Res = yamerl_constr:file(Path), true = is_datasheet(Res),
        erlmachine:success(Res)
    catch E:R ->
            erlmachine:failure(E, R)
    end.

-spec gear(Datasheet::datasheet()) -> 
                  success(assembly()) | failure(term(), term()).
gear(Datasheet) ->
    gear(Datasheet, []).

-spec gear(Datasheet::datasheet(), Exts::list(assembly())) -> 
                  success(assembly()) | failure(term(), term()).
gear(Datasheet, Exts) ->
    io:format("~nDatasheet: ~p~n",[Datasheet]),
    Name = test, Opt = [], ProtName = test, ProtOpt = [],
    erlmachine_factory:gear(Name, Opt, ProtName, ProtOpt, Exts).

-spec shaft(Datasheet::datasheet()) ->
                   success(assembly()) | failure(term(), term()).
shaft(Datasheet) ->
    shaft(Datasheet, []).

-spec shaft(Datasheet::datasheet(), Exts::list(assembly())) ->
                   success(assembly()) | failure(term(), term()).
shaft(Datasheet, Exts) ->
    io:format("~nDatasheet: ~p~n",[Datasheet]),
    Name = test, Opt = [], ProtName = test, ProtOpt = [],
    erlmachine_factory:shaft(Name, Opt, ProtName, ProtOpt, Exts).

-spec axle(Datasheet::datasheet()) ->
                  success(assembly()) | failure(term(), term()).
axle(Datasheet) ->
    axle(Datasheet, []).

-spec axle(Datasheet::datasheet(), Exts::list(assembly())) -> 
                  success(assembly()) | failure(term(), term()).
axle(Datasheet, Exts) ->
    io:format("~nDatasheet: ~p~n",[Datasheet]),
    Name = test, Opt = [], ProtName = test, ProtOpt = [],
    erlmachine_factory:axle(Name, Opt, ProtName, ProtOpt, Exts).

-spec gearbox(Datasheet::datasheet(), Env::term()) ->
                     success(assembly()) | failure(term(), term()).
gearbox(Datasheet, Env) ->
    gearbox(Datasheet, Env, []).

-spec gearbox(Datasheet::datasheet(), Env::term(), Exts::list(assembly())) -> 
                     success(assembly()) | failure(term(), term()).
gearbox(Datasheet, Env, Exts) ->
    io:format("~nDatasheet: ~p~n",[Datasheet]),
    Name = test, Opt = [], ProtName = test, ProtOpt = [],
    erlmachine_factory:gearbox(Name, Opt, ProtName, ProtOpt, Env, Exts).

%% TODO to support Json based specification;

-module(erlmachine_datasheet).

%% API.
-export([file/1]).

-export([gear/2, gear/3]).
-export([shaft/2, shaft/3]).
-export([axle/2, axle/3]).
-export([gearbox/3, gearbox/4]).

%% We assume that this module can provide additional set of tools:
%% 1) Navigation over document;
%% 2) Validate document structure;

-include("erlmachine_datasheet.hrl").
-include("erlmachine_factory.hrl").
-include("erlmachine_system.hrl").

%% I guess the specified module can provide endpoints (gear, shaft, etc..);

-spec file(File::list()) ->
                       datasheet().
file(File) ->
    yamerl_constr:file(File).

-spec gear(File::list(), Label::term(), Parts::list()) -> 
                  assembly().
gear(File, Label, Parts) -> 
    erlmachine_assembly:label(gear(File, Parts), Label).

-spec gear(File::list(), Parts::list()) -> 
                  assembly().
gear(File, Parts) -> 
    Datasheet = file(File), true = is_list(Datasheet),
    io:format("~nDatasheet: ~p~n",[Datasheet]),

    Assembly = erlmachine_factory:gear(Name, Opt, ProtName, ProtOpt, Parts),
    erlmachine_factory:build(Assembly). %% TODO

-spec shaft(File::list(), Label::term(), Parts::list()) -> 
                   assembly().
shaft(File, Label, Parts) -> 
    erlmachine_assembly:label(shaft(File, Parts), Label).

-spec shaft(File::list(), Parts::list()) -> 
                   assembly().
shaft(File, Parts) -> 
    Datasheet = file(File), true = is_list(Datasheet),

    Assembly = erlmachine_factory:shaft(Name, Opt, ProtName, ProtOpt, Parts),
    erlmachine_factory:build(Assembly). %% TODO To decorate by tags, description, etc.;

-spec axle(File::list(), Label::term(), Parts::list()) -> 
                   assembly().
axle(File, Label, Parts) -> 
    erlmachine_assembly:label(axle(File, Parts), Label).

-spec axle(File::list(), Parts::list()) -> 
                  assembly().
axle(File, Parts) -> 
    Datasheet = file(File), true = is_list(Datasheet),

    Assembly = erlmachine_factory:axle(Name, Opt, ProtName, ProtOpt, Parts),
    erlmachine_factory:build(Assembly). %% TODO To decorate by tags, description, etc.;

-spec gearbox(File::list(), Env::term(), Label::term(), Parts::list()) -> 
                  assembly().
gearbox(File, Env, Label, Parts) -> 
    erlmachine_assembly:label(gearbox(File, Env, Parts), Label).

-spec gearbox(File::list(), Env::term(), Parts::list()) -> 
                     assembly().
gearbox(File, Env, Parts) -> 
    Datasheet = file(File), true = is_list(Datasheet),

    Assembly = erlmachine_factory:gearbox(Name, Opt, ProtName, ProtOpt, Env, Parts),
    erlmachine_factory:build(Assembly). %% TODO To decorate by tags, description, etc.;

%% TODO model options;
-spec model(Datasheet::datasheet()) -> model().
model(Datasheet) ->
    Prototype = erlmachine_prototype:prototype(Name, Opt),
    erlmachine_model:model(Name, Opt, Prototype).

-spec assembly(Product, Datasheet::datasheet(), Parts::list()) -> assembly().
assembly() ->
    assembly(Product, Body, Model, Parts).

%% TODO to support Json based specification;

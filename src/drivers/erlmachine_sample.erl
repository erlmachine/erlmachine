-module(erlmachine_sample).

-export([gear/1, gear/2]).
-export([shaft/2]).
-export([axle/2]).
-export([gearbox/3]).

-export([supervisor_prototype/0, worker_prototype/0]).

-include("erlmachine_assembly.hrl").
-include("erlmachine_system.hrl").

%%%===================================================================
%%% Catalogue
%%%===================================================================

-spec gear(Opt::term()) -> assembly().
gear(Opt) ->
    Model = erlmachine_gear_sample,
    erlmachine_factory:gear(Model, Opt).

-spec gear(Opt::term(), Ext::assembly()) -> assembly().
gear(Opt, Ext) ->
    Model = erlmachine_gear_sample,
    erlmachine_factory:gear(Model, Opt, Ext).

-spec shaft(Opt::term(), Exts::list(assembly())) -> assembly().
shaft(Opt, Exts) ->
    Model = erlmachine_shaft_sample,
    erlmachine_factory:shaft(Model, Opt, Exts).

-spec axle(Opt::term(), Exts::list(assembly())) -> assembly().
axle(Opt, Exts) ->
    Model = erlmachine_axle_sample,
    erlmachine_factory:axle(Model, Opt, Exts).

-spec gearbox(Opt::term(), Env::term(), Exts::list(assembly())) -> assembly().
gearbox(Opt, Env, Exts) ->
    Model = erlmachine_gearbox_sample,
    erlmachine_factory:gearbox(Model, Opt, Env, Exts).

supervisor_prototype() ->
    erlmachine_supervisor_sample_prototype.

worker_prototype() ->
    erlmachine_worker_sample_prototype.

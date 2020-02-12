-module(erlmachine_shaft).

-export([
         install/2,
         attach/4, detach/3,
         replace/3,
         transmit/3, rotate/4, rotate/3, load/3,
         accept/3,
         overload/3, block/4,
         uninstall/3
        ]).

-export([form/2, submit/3]).

-export([shaft/0]).

-export([parts/2]).

-export([record_name/0, attributes/0]).

-include("erlmachine_factory.hrl").
-include("erlmachine_system.hrl").

-callback install(SN::serial_no(), IDs::list(term()), State::term(), Opt::term(), Env::list()) -> 
    success(term()) | failure(term(), term(), term()) | failure(term()).

-callback replace(SN::serial_no(), ID::term(), State::term()) -> 
    success(term()) | failure(term(), term(), term()) | failure(term()).

-callback uninstall(SN::serial_no(), Reason::term(), State::term()) -> 
    success(term()) | failure(term(), term(), term()) | failure(term()).

-callback accept(SN::serial_no(), Criteria::criteria(), State::term()) -> 
    success() | failure(term(), term(), term()).

-callback attach(SN::serial_no(), Reg::term(), ID::term(), State::term()) -> 
    success(term()) | failure(term(), term(), term()) | failure(term()).

-callback detach(SN::serial_no(), ID::term(), State::term()) -> 
    success(term()) | failure(term(), term(), term()) | failure(term()).

-callback rotate(SN::serial_no(), ID::term(), Motion::term(), State::term()) -> 
    success(term(), term()) | success(term()) | failure(term(), term(), term()) | failure(term()).

-callback transmit(SN::serial_no(), Motion::term(), State::term()) -> 
    success(term(), term()) | failure(term(), term(), term()) | failure(term()).

-callback load(SN::serial_no(), Load::term(), State::term()) ->
    success(term()) |  success(term(), term()) | failure(term(), term(), term()) | failure(term()).

-callback overload(SN::serial_no(), Load::term(), State::term()) -> 
    success(term()) | failure(term(), term(), term()) | failure(term()).

-callback block(SN::serial_no(), ID::term(), Failure::term(), State::term()) -> 
    success(term()) | failure(term(), term(), term()) | failure(term()).

-callback form(SN::serial_no(), State::term()) ->
    success(term(), term()) | failure(term(), term(), term()) | failure(term()).

-callback submit(SN::serial_no(), Form::term(), State::term()) ->
    success(term()) | failure(term(), term(), term()) | failure(term()).

-optional_callbacks([replace/3, overload/3, block/4]).
-optional_callbacks([form/2, submit/3]).

%% Instead of gear the main puropse of shaft is to transmit power between parts;

-record(shaft, {}).

-type shaft() :: #shaft{}.

-export_type([shaft/0]).

-spec record_name() -> atom().
record_name() ->
    shaft.

-spec attributes() -> list(atom()).
attributes() ->
    record_info(fields, shaft).

-spec shaft() -> shaft().
shaft() ->
    #shaft{ }.

-spec install(GearBox::assembly(), Shaft::assembly()) -> 
                     success(assembly()) | failure(term(), term(), term()).
install(GearBox, Shaft) ->
    ModelName = erlmachine_assembly:model_name(Shaft),
    SN = erlmachine_assembly:serial_no(Shaft),
    Env = erlmachine_gearbox:env(GearBox), 
    Opt = erlmachine_assembly:model_options(Shaft),
    %% We can check exported functions accordingly to this kind of behaviour; 
    %% We are going to add error handling later; 
    IDs = [erlmachine_assembly:label(Part)|| Part <- erlmachine_assembly:parts(Shaft)], 

    {ok, State} = ModelName:install(SN, IDs, state(Shaft), Opt, Env),
    
    %% We are going to add error handling later; 
    Rel = state(Shaft, State), 
    erlmachine_assembly:installed(GearBox, Rel),
    {ok, Rel}.

-spec attach(GearBox::assembly(), Shaft::assembly(), Register::term(), Extension::assembly()) ->
                    success(assembly(), assembly()) | failure(term(), term(), term()).
attach(GearBox, Shaft, Register, Extension) ->
    ModelName= erlmachine_assembly:model_name(Shaft),
    SN = erlmachine_assembly:serial_no(Shaft), ID = erlmachine_assembly:serial_no(Extension),

    Mod = ModelName, Fun = attach, Args = [SN, Register, ID, state(Shaft)],
    {ok, State} = erlmachine:optional_callback(Mod, Fun, Args, erlmachine:success(state(Shaft))),

    Part = Extension,
    Rel = erlmachine_assembly:add(state(Shaft, State), Part),
    erlmachine_assembly:attached(GearBox, Rel, Part),
    {ok, Part, Rel}.

-spec detach(GearBox::assembly(), Shaft::assembly(), ID::serial_no()) ->
                    success(assembly()) | failure(term(), term(), term()).
detach(GearBox, Shaft, ID) ->
    ModelName= erlmachine_assembly:model_name(Shaft),
    SN = erlmachine_assembly:serial_no(Shaft),

    Mod = ModelName, Fun = detach, Args = [SN, ID, state(Shaft)],
    {ok, State} = erlmachine:optional_callback(Mod, Fun, Args, erlmachine:success(state(Shaft))),

    Rel = erlmachine_assembly:remove(state(Shaft, State), ID),
    erlmachine_assembly:detached(GearBox, Rel, ID),
    {ok, Rel}.

-spec replace(GearBox::assembly(), Shaft::assembly(), Part::assembly()) ->
                     success(assembly()) | failure(term(), term(), term()).
replace(GearBox, Shaft, Part) ->
    ModelName = erlmachine_assembly:model_name(Shaft),
    SN = erlmachine_assembly:serial_no(Shaft), ID = erlmachine_assembly:serial_no(Part),

    Mod = ModelName, Fun = replace, Args = [SN, ID, state(Shaft)],
    {ok, State} = erlmachine:optional_callback(Mod, Fun, Args, erlmachine:success(state(Shaft))),

    Rel = state(Shaft, State),
    erlmachine_assembly:replaced(GearBox, Rel, Part),
    {ok, Rel}.

-spec accept(GearBox::assembly(), Shaft::assembly(), Criteria::term()) ->
                    success()| failure(term(), term(), term()).
accept(GearBox, Shaft, Criteria) ->
    ModelName = erlmachine_assembly:model_name(Shaft),
    SN = erlmachine_assembly:serial_no(Shaft),
   
    {ok, Res, State} = ModelName:accept(SN, Criteria, state(Shaft)),

    Rel = state(Shaft, State),
    case Res of 
        ok ->
            erlmachine_factory:accepted(GearBox, Rel, Criteria);
        _ ->
            erlmachine_factory:rejected(GearBox, Rel, Criteria, Res)
    end,
    {ok, Res, Shaft}.

-spec rotate(GearBox::assembly(), Shaft::assembly(), ID::serial_no(), Motion::term()) ->
                    success(assembly()) | failure(term(), term(), term()).
rotate(GearBox, Shaft, ID, Motion) ->
    ModelName = erlmachine_assembly:model_name(Shaft),
    SN = erlmachine_assembly:serial_no(Shaft),
    Part = erlmachine_assembly:part(Shaft, ID),

    case ModelName:rotate(SN, ID, Motion, state(Shaft)) of 
        {ok, Res, State} -> 
            erlmachine_transmission:rotation(GearBox, Part, Res),
            {ok, state(Shaft, State)};
        {ok, State} -> 
            {ok, state(Shaft, State)}
        end.

-spec rotate(GearBox::assembly(), Shaft::assembly(), Motion::term()) ->
                    success(assembly()) | failure(term(), term(), term()).
rotate(GearBox, Shaft, Motion) ->
    Fun = fun (Part, {ok, Acc}) -> 
                  ID = erlmachine_assembly:label(Part),
                  rotate(GearBox, Acc, ID, Motion)
          end,
    lists:foldl(Fun, {ok, Shaft}, erlmachine_assembly:parts(Shaft)).

-spec load(GearBox::assembly(), Shaft::assembly(), Load::term()) ->
                  success(assembly()) | failure(term(), term(), term()).
load(GearBox, Shaft, Load) ->
    ModelName = erlmachine_assembly:model_name(Shaft),
    SN = erlmachine_assembly:serial_no(Shaft),

    case ModelName:load(SN, Load, state(Shaft)) of 
        {ok, Res, State} -> 
            rotate(GearBox, state(Shaft, State), Res);
        {ok, State} -> 
            {ok, state(Shaft, State)}
    end.

-spec transmit(GearBox::assembly(), Shaft::assembly(), Motion::term()) ->
                    success(term(), assembly()) | failure(term(), term(), assembly()).
transmit(_GearBox, Shaft, Motion) ->
    ModelName = erlmachine_assembly:model_name(Shaft),
    SN = erlmachine_assembly:serial_no(Shaft),
 
    {ok, Res, Body} = ModelName:tranmsit(SN, Motion, state(Shaft)),
    
    Rel = state(Shaft, Body),
    {ok, Res, Rel}.

-spec overload(GearBox::assembly(), Shaft::assembly(), Load::term()) ->
                      success(assembly()) | failure(term(), term(), term()).
overload(GearBox, Shaft, Load) ->
    ModelName = erlmachine_assembly:model_name(Shaft),
    SN = erlmachine_assembly:serial_no(Shaft),
  
    Mod = ModelName, Fun = overload, Args = [SN, Load, state(Shaft)],
    {ok, State} = erlmachine:optional_callback(Mod, Fun, Args, erlmachine:success(state(Shaft))),

    Rel = state(Shaft, State),
    erlmachine_assembly:overloaded(GearBox, Rel, Load),
    {ok, Rel}.

-spec block(GearBox::assembly(), Shaft::assembly(), Part::assembly(), Failure::term()) ->
                      success(assembly()) | failure(term(), term(), assembly()).
block(GearBox, Shaft, Part, Failure) ->
    ModelName = erlmachine_assembly:model_name(Shaft),
    SN = erlmachine_assembly:serial_no(Shaft), ID = erlmachine_assembly:label(Part),

    Mod = ModelName, Fun = block, Args = [SN, ID, Failure, state(Shaft)],
    {ok, State} = erlmachine:optional_callback(Mod, Fun, Args, erlmachine:success(state(Shaft))),

    Rel = state(Shaft, State),
    erlmachine_assembly:blocked(GearBox, Rel, Part, Failure),
    {ok, Rel}.

-spec uninstall(GearBox::assembly(), Shaft::assembly(), Reason::term()) -> 
                       ok.
uninstall(GearBox, Shaft, Reason) ->
    ModelName = erlmachine_assembly:model_name(Shaft),
    SN = erlmachine_assembly:serial_no(Shaft),
  
    {ok, State} = ModelName:uninstall(SN, Reason, state(Shaft)),
    
    Rel = state(Shaft, State),
    erlmachine_assembly:uninstalled(GearBox, Rel, Reason),
    ok.

-spec form(GearBox::assembly(), Shaft::assembly()) ->
                  success(term(), assembly()) | failure(term(), term(), term()).
form(_GearBox, Shaft) ->
    ModelName = erlmachine_assembly:model_name(Shaft),
    SN = erlmachine_assembly:serial_no(Shaft),

    Mod = ModelName, Fun = form, Args = [SN, state(Shaft)],
    Def = erlmachine:success([], state(Shaft)),

    {ok, Form, State} = erlmachine:optional_callback(Mod, Fun, Args, Def),
    {ok, Form, state(Shaft, State)}.

-spec submit(GearBox::assembly(), Shaft::assembly(), Form::term()) ->
                    success(term(), assembly()) | failure(term(), term(), term()).
submit(_GearBox, Shaft, Form) ->
    ModelName = erlmachine_assembly:model_name(Shaft),
    SN = erlmachine_assembly:serial_no(Shaft),

    {ok, Res, State} = ModelName:submit(SN, Form, state(Shaft)),
    {ok, Res, state(Shaft, State)}.

-spec state(Shaft::assembly()) -> term().
state(Shaft) ->
    erlmachine_assembly:body(Shaft).

-spec state(Shaft::assembly(), State::term()) -> assembly().
state(Shaft, State) ->
    erlmachine_assembly:body(Shaft, State).

-spec parts(Shaft::assembly(), Parts::list(assembly())) -> assembly().
parts(Shaft, Parts) ->
    erlmachine_assembly:parts(Shaft, Parts).

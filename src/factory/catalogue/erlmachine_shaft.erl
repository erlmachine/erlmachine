-module(erlmachine_shaft).

-export([
         install/2,
         attach/4, detach/3,
         replace/3,
         transmit/3, rotate/4, rotate/3,
         accept/3,
         overload/3, block/4,
         uninstall/3
        ]).

-export([
         shaft/1,
         body/1, body/2
        ]).

-export([parts/2]).

-include("erlmachine_factory.hrl").
-include("erlmachine_system.hrl").

-callback install(SN::serial_no(), IDs::list(serial_no()), Body::term(), Options::term(), Env::list()) -> 
    success(term()) | failure(term(), term(), term()) | failure(term()).

-callback replace(SN::serial_no(), ID::serial_no(), Body::term()) -> 
    success(term()) | failure(term(), term(), term()) | failure(term()).

-callback uninstall(SN::serial_no(), Reason::term(), Body::term()) -> 
    success(term()) | failure(term(), term(), term()) | failure(term()).

-callback accept(SN::serial_no(), Criteria::criteria(), Body::term()) -> 
    success() | failure(term(), term(), term()).

-callback attach(SN::serial_no(), Register::term(), ID::serial_no(), Body::term()) -> 
    success(term()) | failure(term(), term(), term()) | failure(term()).

-callback detach(SN::serial_no(), ID::serial_no(), Body::term()) -> 
    success(term()) | failure(term(), term(), term()) | failure(term()).

-callback rotate(SN::serial_no(), ID::serial_no(), Motion::term(), Body::term()) -> 
    success(term(), term()) | success(term()) | failure(term(), term(), term()) | failure(term()).

-callback transmit(SN::serial_no(), Motion::term(), Body::term()) -> 
    success(term(), term()) | failure(term(), term(), term()) | failure(term()).

-callback overload(SN::serial_no(), Load::term(), Body::term()) -> 
    success(term()) | failure(term(), term(), term()) | failure(term()).

-callback block(SN::serial_no(), ID::serial_no(), Failure::term(), Body::term()) -> 
    success(term()) | failure(term(), term(), term()) | failure(term()).

%% Instead of gear the main puropse of shaft is to transmit power between parts;

-record(shaft, {body::term()}).

-type shaft() :: #shaft{}.

-export_type([shaft/0]).

-spec shaft(Body::term()) -> shaft().
shaft(Body) ->
    #shaft{body=Body}.

-spec install(GearBox::assembly(), Shaft::assembly()) -> 
                     success(Release::assembly()) | failure(E::term(), R::term(), Rejected::assembly()).
install(GearBox, Shaft) ->
    ModelName = erlmachine_assembly:model_name(Shaft),
    SN = erlmachine_assembly:serial_no(Shaft),
    Env = erlmachine_gearbox:env(GearBox), 
    Options = erlmachine_assembly:model_options(Shaft),
    %% We can check exported functions accordingly to this kind of behaviour; 
    %% We are going to add error handling later; 
    IDs = [erlmachine_assembly:serial_no(Part)|| Part <- erlmachine_assembly:parts(Shaft)], 

    {ok, Body} = ModelName:install(SN, IDs, body(Shaft), Options, Env),
    
    %% We are going to add error handling later; 
    Release = body(Shaft, Body), 
    erlmachine_assembly:installed(GearBox, Release),
    {ok, Release}.

-spec attach(GearBox::assembly(), Shaft::assembly(), Register::term(), Extension::assembly()) ->
                    success(Part::assembly(), Release::assembly()) | failure(E::term(), R::term(), Rejected::assembly()).
attach(GearBox, Shaft, Register, Extension) ->
    ModelName= erlmachine_assembly:model_name(Shaft),
    SN = erlmachine_assembly:serial_no(Shaft), ID = erlmachine_assembly:serial_no(Extension),

    {ok, Body} = ModelName:attach(SN, Register, ID, body(Shaft)),
    
    Part = Extension,
    %% TODO At this place we can provide modification layer over attach;
    Release = erlmachine_assembly:add_part(body(Shaft, Body), Part),
    erlmachine_assembly:attached(GearBox, Release, Part),
    {ok, Part, Release}.

-spec detach(GearBox::assembly(), Shaft::assembly(), ID::serial_no()) ->
                    success(Release::assembly()) | failure(E::term(), R::term(),  Rejected::assembly()).
detach(GearBox, Shaft, ID) ->
    ModelName= erlmachine_assembly:model_name(Shaft),
    SN = erlmachine_assembly:serial_no(Shaft),
    %% At that place we need to find Part inside assembly by SN and transmit;
  
    {ok, Body} = ModelName:detach(SN, ID, body(Shaft)),
    
    Release = erlmachine_assembly:remove_part(body(Shaft, Body), ID),
    erlmachine_assembly:detached(GearBox, Release, ID),
    {ok, Release}.

-spec replace(GearBox::assembly(), Shaft::assembly(), Part::assembly()) ->
                     success(Release::assembly()) | failure(E::term(), R::term(), Rejected::assembly()).
replace(GearBox, Shaft, Part) ->
    ModelName = erlmachine_assembly:model_name(Shaft),
    SN = erlmachine_assembly:serial_no(Shaft), ID = erlmachine_assembly:serial_no(Part),
  
    {ok, Body} = ModelName:replace(SN, ID, body(Shaft)),
    
    Release = body(Shaft, Body),
    erlmachine_assembly:replaced(GearBox, Release, Part),
    {ok, Release}.

-spec accept(GearBox::assembly(), Shaft::assembly(), Criteria::term()) ->
                    success()| failure(E::term(), R::term(), S::term()).
accept(GearBox, Shaft, Criteria) ->
    ModelName = erlmachine_assembly:model_name(Shaft),
    SN = erlmachine_assembly:serial_no(Shaft),
   
    Result = ModelName:accept(SN, Criteria, body(Shaft)),
    case Result of 
        ok ->
            erlmachine_factory:accepted(GearBox, Shaft, Criteria);
        _ ->
            erlmachine_factory:rejected(GearBox, Shaft, Criteria, Result)
    end,
    Result.

-spec rotate(GearBox::assembly(), Shaft::assembly(), ID::serial_no(), Motion::term()) ->
                    success(Release::assembly()) | failure(E::term(), R::term(), Rejected::assembly()).
rotate(GearBox, Shaft, ID, Motion) ->
    ModelName = erlmachine_assembly:model_name(Shaft),
    SN = erlmachine_assembly:serial_no(Shaft),
    Part = erlmachine_assembly:get_part(Shaft, ID),
  
    ReleaseBody = 
        case ModelName:rotate(SN, ID, Motion, body(Shaft)) of 
            {ok, Result, Body} -> 
                erlmachine_transmission:rotate(GearBox, Part, Result),
                Body;
            {ok, Body} -> 
                Body 
        end,
    
    Release = body(Shaft, ReleaseBody),
    {ok, Release}.

-spec rotate(GearBox::assembly(), Shaft::assembly(), Motion::term()) ->
                    success(Release::assembly()) | failure(E::term(), R::term(), Rejected::assembly()).
rotate(GearBox, Shaft, Motion) ->
    Parts = erlmachine_assembly:parts(Shaft),
    {ok, Release} = lists:foldl(
      fun (Part, {ok, ShaftState}) ->
              ID = erlmachine_assembly:serial_no(Part),
              rotate(GearBox, ShaftState, ID, Motion)
      end,
      {ok, Shaft},
      Parts),
    {ok, Release}.

-spec transmit(GearBox::assembly(), Shaft::assembly(), Motion::term()) ->
                    success(Result::term(), Release::assembly()) | failure(E::term(), R::term(), Rejected::assembly()).
transmit(_GearBox, Shaft, Motion) ->
    ModelName = erlmachine_assembly:model_name(Shaft),
    SN = erlmachine_assembly:serial_no(Shaft),
 
    {ok, Result, Body} = ModelName:tranmsit(SN, Motion, body(Shaft)),
    
    Release = body(Shaft, Body),
    {ok, Result, Release}.

-spec overload(GearBox::assembly(), Shaft::assembly(), Load::term()) ->
                      success(Release::assembly()) | failure(E::term(), R::term(), Reject::assembly()).
overload(GearBox, Shaft, Load) ->
    ModelName = erlmachine_assembly:model_name(Shaft),
    SN = erlmachine_assembly:serial_no(Shaft),
  
    {ok, Body} = ModelName:overload(SN, Load, body(Shaft)),
    
    Release = body(Shaft, Body),
    erlmachine_assembly:overloaded(GearBox, Release, Load),
    {ok, Release}.

-spec block(GearBox::assembly(), Shaft::assembly(), Part::assembly(), Failure::term()) ->
                      success(Release::assembly()) | failure(E::term(), R::term(), Reject::assembly()).
block(GearBox, Shaft, Part, Failure) ->
    ModelName = erlmachine_assembly:model_name(Shaft),
    SN = erlmachine_assembly:serial_no(Shaft), ID = erlmachine_assembly:serial_no(Part),
  
    {ok, Body} = ModelName:block(SN, ID, Failure, body(Shaft)),
    
    Release = body(Shaft, Body),
    erlmachine_assembly:blocked(GearBox, Release, Part, Failure),
    {ok, Release}.

-spec uninstall(GearBox::assembly(), Shaft::assembly(), Reason::term()) -> 
    ok.
uninstall(GearBox, Shaft, Reason) ->
    ModelName = erlmachine_assembly:model_name(Shaft),
    SN = erlmachine_assembly:serial_no(Shaft),
  
    {ok, Body} = ModelName:uninstall(SN, Reason, body(Shaft)),
    
    Release = body(Shaft, Body),
    erlmachine_assembly:uninstalled(GearBox, Release, Reason),
    ok.

-spec body(Shaft::assembly()) -> Body::term().
body(Shaft) ->
    Product = erlmachine_assembly:product(Shaft),
    Product#shaft.body.

-spec body(Shaft::assembly(), Body::term()) -> Release::assembly().
body(Shaft, Body) ->
    Product = erlmachine_assembly:product(Shaft),
    erlmachine_assembly:product(Shaft, Product#shaft{body=Body}).

-spec parts(Shaft::assembly(), Parts::list(assembly())) -> Release::assembly().
parts(Shaft, Parts) ->
    Release = erlmachine_assembly:parts(Shaft, Parts),
    Release.

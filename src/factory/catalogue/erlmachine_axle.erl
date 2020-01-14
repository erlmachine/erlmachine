-module(erlmachine_axle).

-export([
         install/2,
         attach/4, detach/3,
         accept/3,
         uninstall/3
        ]).

-export([
         axle/1,
         body/1, body/2
        ]).

-export([parts/2]).

-include("erlmachine_factory.hrl").
-include("erlmachine_system.hrl").

-callback install(SN::serial_no(), IDs::list(serial_no()), Body::term(), Options::term(), Env::list()) -> 
    success(term()) | failure(term(), term(), term()) | failure(term()).

-callback uninstall(SN::serial_no(), Reason::term(), Body::term()) -> 
    success(term()) | failure(term(), term(), term()) | failure(term()).

-callback accept(SN::serial_no(), Criteria::criteria(), Body::term()) -> 
    success() | failure(term(), term(), term()).

-callback attach(SN::serial_no(), Register::term(), ID::serial_no(), Body::term()) -> 
    success(term()) | failure(term(), term(), term()) | failure(term()).

-callback detach(SN::serial_no(), ID::serial_no(), Body::term()) -> 
    success(term()) | failure(term(), term(), term()) | failure(term()).

-record(axle, { body::term() }).

%% I am thinking about graph ipmlementation of body;
-type axle() :: #axle{}.

-export_type([axle/0]).

-spec axle(Body::term()) -> axle().
axle(Body) ->
    #axle{body=Body}.

-spec install(GearBox::assembly(), Axle::assembly()) -> 
                     success(Release::assembly()) | failure(E::term(), R::term(), Rejected::assembly()).
install(GearBox, Axle) ->
    ModelName = erlmachine_assembly:model_name(Axle),
    SN = erlmachine_assembly:serial_no(Axle),
    Env = erlmachine_gearbox:env(GearBox), 
    Options = erlmachine_assembly:model_options(Axle),
    IDs = [erlmachine_assembly:serial_no(Part)|| Part <- erlmachine_assembly:parts(Axle)], 

    {ok, Body} = ModelName:install(SN, IDs, body(Axle), Options, Env),
    
    %% We are going to add error handling later; 
    Release = body(Axle, Body),
    erlmachine_assembly:installed(GearBox, Release),
    {ok, Release}.

-spec attach(GearBox::assembly(), Axle::assembly(), Register::term(), Extension::assembly()) ->
                    success(Part::assembly(), Release::assembly()) | failure(E::term(), R::term(), Rejected::assembly()).
attach(GearBox, Axle, Register, Extension) ->
    ModelName = erlmachine_assembly:model_name(Axle),
    SN = erlmachine_assembly:serial_no(Axle), ID = erlmachine_assembly:serial_no(Extension),
    
    {ok, Body} = ModelName:attach(SN, Register, ID, body(Axle)),
    
    Part = erlmachine_assembly:mounted(Extension, Axle),
    Release = erlmachine_assembly:add_part(body(Axle, Body), Part),
    erlmachine_assembly:attached(GearBox, Release, Part),
    {ok, Part, Release}. %% TODO

-spec detach(GearBox::assembly(), Axle::assembly(), ID::serial_no()) ->
                    success(Release::assembly()) | failure(E::term(), R::term(), Rejected::assembly()).
detach(GearBox, Axle, ID) ->
    ModelName = erlmachine_assembly:model_name(Axle),
    SN = erlmachine_assembly:serial_no(Axle),

    {ok, Body} = ModelName:detach(SN, ID, body(Axle)),
    
    Release = erlmachine_assembly:remove_part(body(Axle, Body), ID),
    erlmachine_assembly:detached(GearBox, Release, ID),
    {ok, Release}. %% TODO

-spec accept(GearBox::assembly(), Axle::assembly(), Criteria::criteria()) ->
                    success() | failure(E::term(), R::term(), S::term()).
accept(GearBox, Axle, Criteria) ->
    ModelName = erlmachine_assembly:model_name(Axle),
    SN = erlmachine_assembly:serial_no(Axle),
    
    {ok, Result, _} = ModelName:accept(SN, Criteria, body(Axle)),
    case Result of 
        ok ->
            erlmachine_factory:accepted(GearBox, Axle, Criteria);
        _ ->
            erlmachine_factory:rejected(GearBox, Axle, Criteria, Result)
    end,
    {ok, Result, Axle}.

-spec uninstall(GearBox::assembly(), Axle::assembly(), Reason::term()) -> 
                       ok.
uninstall(GearBox, Axle, Reason) ->
    ModelName = erlmachine_assembly:model_name(Axle),
    SN = erlmachine_assembly:serial_no(Axle),
    
    {ok, Body} = ModelName:uninstall(SN, Reason, body(Axle)),
    
    Release = body(Axle, Body),
    erlmachine_assembly:uninstalled(GearBox, Release, Reason),
    ok.

-spec body(Axle::assembly()) -> Body::term().
body(Axle) ->
    Product = erlmachine_assembly:product(Axle),
    Product#axle.body.

-spec body(Axle::assembly(), Body::term()) -> Release::assembly().
body(Axle, Body) ->
    Product = erlmachine_assembly:product(Axle),
    erlmachine_assembly:product(Axle, Product#axle{ body=Body }).

-spec parts(Axle::assembly(), Parts::list(assembly())) -> Release::assembly().
parts(Axle, Parts) ->
    Mounted = [erlmachine_assembly:mounted(Part, Axle)|| Part <- Parts],
    Release = erlmachine_assembly:parts(Axle, Mounted),
    Release.

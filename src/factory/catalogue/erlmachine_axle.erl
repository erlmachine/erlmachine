-module(erlmachine_axle).

-export([
         install/2,
         mount/3, unmount/3,
         accept/3,
         uninstall/3
        ]).

-export([
         axle/1,
         body/1, body/2
        ]).

-export([specs/2, spec/3]).

-include("erlmachine_factory.hrl").
-include("erlmachine_system.hrl").

-callback install(SN::serial_no(), IDs::list(serial_no()), Body::term(), Options::term(), Env::list()) -> 
    success(term()) | failure(term(), term(), term()) | failure(term()).

-callback uninstall(SN::serial_no(), Reason::term(), Body::term()) -> 
    success(term()) | failure(term(), term(), term()) | failure(term()).

-callback accept(SN::serial_no(), Criteria::term(), Body::term()) -> 
    success(term(), term()) | failure(term(), term(), term()) | failure(term()).

-callback mount(SN::serial_no(), ID::serial_no(), Body::term()) -> 
    success(term()) | failure(term(), term(), term()) | failure(term()).

-callback unmount(SN::serial_no(), ID::serial_no(), Body::term()) -> 
    success(term()) | failure(term(), term(), term()) | failure(term()).

-record(axle, {body::term()}).

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
    Mounted = erlmachine_assembly:mounted(Axle),
    (Mounted /= undefined) andalso (erlmachine_assembly:prototype_name(Mounted)):installed(SN, Mounted, Release),
    (Mounted == GearBox) orelse (erlmachine_assembly:prototype_name(GearBox)):installed(SN, GearBox, Release),
    {ok, Release}.

-spec mount(GearBox::assembly(), Axle::assembly(), Part::assembly()) ->
                    success(Release::assembly()) | failure(E::term(), R::term(), Rejected::assembly()).
mount(_GearBox, Axle, _Part) ->
    {ok, Axle}. %% TODO

-spec unmount(GearBox::assembly(), Axle::assembly(), ID::serial_no()) ->
                    success(Release::assembly()) | failure(E::term(), R::term(), Rejected::assembly()).
unmount(_GearBox, Axle, _Part) ->
    {ok, Axle}. %% TODO

-spec accept(GearBox::assembly(), Axle::assembly(), Criteria::term()) ->
                    success(Report::term(), Release::assembly())| failure(E::term(), R::term(), Rejected::assembly()).
accept(GearBox, Axle, Criteria) ->
    ModelName = erlmachine_assembly:model_name(Axle),
    SN = erlmachine_assembly:serial_no(Axle),
    {Tag, Result, Body} = ModelName:accept(SN, Criteria, body(Axle)),
    Release = body(Axle, Body),
    case Tag of 
        ok ->
            Report = Result,
            (erlmachine_assembly:prototype_name(GearBox)):accepted(SN, GearBox, Release, Criteria, Report),
            {ok, Result, Release};
        error ->
            {_, Report} = Result,
            (erlmachine_assembly:prototype_name(GearBox)):rejected(SN, GearBox, Release, Criteria, Report),
            {error, Result, Release} 
    end.

-spec uninstall(GearBox::assembly(), Axle::assembly(), Reason::term()) -> 
                       ok.
uninstall(GearBox, Axle, Reason) ->
    ModelName = erlmachine_assembly:model_name(Axle),
    SN = erlmachine_assembly:serial_no(Axle),
    {ok, Body} = ModelName:uninstall(SN, Reason, body(Axle)),
    Release = body(Axle, Body),
    Mounted = erlmachine_assembly:mounted(Axle),
    (Mounted /= undefined) andalso (erlmachine_assembly:prototype_name(Mounted)):uninstalled(SN, Mounted, Release, Reason),
    (Mounted == GearBox) orelse (erlmachine_assembly:prototype_name(GearBox)):uninstalled(SN, GearBox, Release, Reason),
    ok.

-spec body(Axle::assembly()) -> Body::term().
body(Axle) ->
    Product = erlmachine_assembly:product(Axle),
    Product#axle.body.

-spec body(Axle::assembly(), Body::term()) -> Release::assembly().
body(Axle, Body) ->
    Product = erlmachine_assembly:product(Axle),
    erlmachine_assembly:product(Axle, Product#axle{body=Body}).

-spec spec(GearBox::assembly(), Axle::assembly(), Part::assembly()) -> map().
spec(GearBox, Axle, Part) ->
    Spec = erlmachine_assembly:spec(GearBox, erlmachine_assembly:mounted(Part, Axle)),
    Spec.

-spec specs(GearBox::assembly(), Axle::assembly()) -> list(map()).
specs(GearBox, Axle) ->
    Parts = erlmachine_assembly:parts(Axle),
    Specs = [spec(GearBox, Axle, Part)|| Part <- Parts],
    Specs.

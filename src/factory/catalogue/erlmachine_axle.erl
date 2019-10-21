-module(erlmachine_axle).

-export([
         install/2,
         accept/3,
         attach/3, detach/3,
         uninstall/3
        ]).

-export([
         axle/1,
         parts/1, parts/2,
         specs/1, specs/2,
         body/1, body/2,
         mount/1, mount/2
        ]).

-include("erlmachine_factory.hrl").
-include("erlmachine_system.hrl").

-callback install(SN::serial_no(), Body::term(), Options::term(), Env::list()) -> 
    success(term()) | failure(term(), term(), term()) | failure(term()).

-callback uninstall(SN::serial_no(), Reason::term(), Body::term()) -> 
    success(term()) | failure(term(), term(), term()) | failure(term()).

-callback accept(SN::serial_no(), Criteria::term(), Body::term()) -> 
    success(term(), term()) | failure(term(), term(), term()) | failure(term()).

-callback attach(SN::serial_no(), ID::serial_no(), Body::term()) -> 
    success(term()) | failure(term(), term(), term()) | failure(term()).

-callback detach(SN::serial_no(), ID::serial_no(), Body::term()) -> 
    success(term()) | failure(term(), term(), term()) | failure(term()).

-record(axle, {body::term(), specs=[]::list(map())}).

-type axle() :: #axle{}.

-export_type([axle/0]).

-spec axle(Body::term()) -> axle().
axle(Body) ->
    #axle{body=Body}.

-spec install(GearBox::assembly(), Axle::assembly()) -> 
                     success(Release::assembly()) | failure(E::term(), R::term(), Rejected::assembly()).
install(GearBox, Axle) ->
    Mount = mount(Axle),
    {ok, Body} = erlmachine_assembly:install_model(Axle),
    %% We are going to add error handling later; 
    Release = body(Axle, Body),
    (Mount /= undefined) andalso erlmachine_assembly:installed(Mount, Release),
    (Mount == GearBox) orelse erlmachine_assembly:installed(GearBox, Release),
    {ok, Release}.

-spec attach(GearBox::assembly(), Axle::assembly(), Part::assembly()) ->
                    success(Release::assembly()) | failure(E::term(), R::term(), Rejected::assembly()).
attach(_GearBox, Axle, _Part) ->
    {ok, Axle}. %% TODO

-spec detach(GearBox::assembly(), Axle::assembly(), ID::serial_no()) ->
                    success(Release::assembly()) | failure(E::term(), R::term(), Rejected::assembly()).
detach(_GearBox, Axle, _Part) ->
    {ok, Axle}. %% TODO

-spec uninstall(GearBox::assembly(), Axle::assembly(), Reason::term()) -> 
                       ok.
uninstall(GearBox, Axle, Reason) ->
    Mount = mount(Axle),
    {ok, Body} = erlmachine_assembly:uninstall_model(Axle, Reason, body(Axle)),
    Release = body(Axle, Body),
    (Mount /= undefined) andalso erlmachine_assembly:uninstalled(Mount, Reason, Release),
    (Mount == GearBox) orelse erlmachine_assembly:uninstalled(GearBox, Reason, Release),
    ok.

-spec accept(GearBox::assembly(), Axle::assembly(), Criteria::term()) ->
                    success(Report::term(), Release::assembly()) | failure(E::term(), R::term(), Rejected::assembly()).
accept(GearBox, Axle, Criteria) ->
    {Tag, Result, Body} = erlmachine_assembly:accept_model(Axle, Criteria, body(Axle)),
    Release = body(Axle, Body),
    case Tag of 
        ok ->
            Report = Result,
            erlmachine_assembly:accepted(GearBox, Release, Criteria, Report),
            {ok, Result, Release};
        error ->
            {_, Report} = Result,
            erlmachine_assembly:rejected(GearBox, Release, Criteria, Report),
            {error, Result, Release} 
    end.


-spec specs(Axle::assembly()) -> list(map()).
specs(Axle) ->
    %% Procs::list(map()),
    Product = erlmachine_assembly:product(Axle),
    Product#axle.specs.

-spec specs(Axle::assembly(), Specs::list(map())) -> Release::assembly().
specs(Axle, Specs) ->
    Product = erlmachine_assembly:product(Axle),
    erlmachine_assembly:product(Axle, Product#axle{specs=Specs}).

-spec body(Axle::assembly()) -> Body::term().
body(Axle) ->
    Product = erlmachine_assembly:product(Axle),
    Product#axle.body.

-spec body(Axle::assembly(), Body::term()) -> Release::assembly().
body(Axle, Body) ->
    Product = erlmachine_assembly:product(Axle),
    erlmachine_assembly:product(Axle, Product#axle{body=Body}).

%%#{id => child_id(),       % mandatory
%%start => mfargs(),      % mandatory
%%restart => restart(),   % optional
%%shutdown => shutdown(), % optional
%%type => worker(),       % optional
%%modules => modules()}   % optional

%% processes need to be instantiated by builder before;

%% process_flag(trap_exit, true), Needs to be passed by default;

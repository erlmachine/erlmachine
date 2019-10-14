-module(erlmachine_shaft).

-export([
         install_model/2,
         attach_model/3, detach_model/3,
         replace_model/3,
         transmit_model/4, rotate_model/3, rotate/2,
         call/3, cast/3, info/3,
         accept_model/3,
         overload_model/3, block_model/4,
         uninstall_model/3
        ]).

-export([
         parts/1, parts/2, 
         body/1, body/2,
         mount/1, mount/2
        ]).

-include("erlmachine_factory.hrl").
-include("erlmachine_system.hrl").

%% Instead of gear the main puropse of shaft is to transmit power between parts;

-spec install_model(GearBox::assembly(), Shaft::assembly()) -> 
                     success(Release::assembly()) | failure(E::term(), R::term(), Rejected::assembly()).
install_model(GearBox, Shaft) ->
    Mount = mount(Shaft),
    {ok, Body} = erlmachine_assembly:install_model(Shaft),
    %% We are going to add error handling later; 
    Release = body(Shaft, Body),
    (Mount /= undefined) andalso erlmachine_assembly:installed(Mount, Release),
    (Mount == GearBox) orelse erlmachine_assembly:installed(GearBox, Release),
    {ok, Release}.

-spec attach_model(GearBox::assembly(), Shaft::assembly(), Part::assembly()) ->
                    success(Release::assembly()) | failure(E::term(), R::term(),  Rejected::assembly()).
attach_model(GearBox, Shaft, Part) ->
    Parts = erlmachine_assembly:attach(parts(Shaft), Part),
    {ok, Body} = erlmachine_transmission:attach_model(Shaft, Part, body(Shaft)),
    Release = parts(body(Shaft, Body), Parts),
    erlmachine_transmission:attached(GearBox, Release, Part),
    {ok, Release}.

-spec detach_model(GearBox::assembly(), Shaft::assembly(), ID::serial_no()) ->
                    success(Release::assembly()) | failure(E::term(), R::term(),  Rejected::assembly()).
detach_model(GearBox, Shaft, ID) ->
    %% At that place we need to find Part inside assembly by SN and transmit;
    {Part, Parts} = erlmachine_assembly:detach(parts(Shaft), ID),
    {ok, Body} = erlmachine_transmission:attach_model(Shaft, Part, body(Shaft)),
    Release = parts(body(Shaft, Body), Parts),
    erlmachine_transmission:attached(GearBox, Release, Part),
    {ok, Release}.

-spec replace_model(GearBox::assembly(), Shaft::assembly(), Repair::assembly()) ->
                     success(Release::assembly()) | failure(E::term(), R::term(), Rejected::assembly()).
replace_model(GearBox, Shaft, Repair) ->
    {ok, Body} = erlmachine_assembly:replace_model(Shaft, Repair, body(Shaft)),
    Release = body(Shaft, Body),
    erlmachine_assembly:replaced(GearBox, Release, Repair),
    {ok, Repair}.

%% Potentially transmit will be able to provide chained processing over list of elements;
-spec transmit_model(GearBox::assembly(), Shaft::assembly(), Part::assembly(), Motion::term()) ->
                      success(Result::term(), Release::assembly()) | failure(E::term(), R::term(), Rejected::assembly()).
transmit_model(_GearBox, Shaft, Part, Motion) ->
    {ok, Result, Body} = erlmachine_transmission:transmit_model(Shaft, Part, Motion, body(body)),
    Release = body(Shaft, Body),
    {ok, Result, Release}.

-spec call(GearBox::assembly(), Shaft::assembly(), Req::term()) -> 
                  ignore.
call(_Gearbox, _Shaft, _Req) -> 
    ignore.

-spec cast(GearBox::assembly(), Shaft::assembly(), Message::term()) -> 
                  ignore.
cast(_Gearbox, _Shaft, _Message) -> 
    ignore.

-spec accept_model(GearBox::assembly(), Shaft::assembly(), Criteria::term()) ->
                    success(Report::term(), Release::assembly())| failure(E::term(), R::term(), Rejected::assembly()).
accept_model(GearBox, Shaft, Criteria) ->
    {Tag, Result, Body} = erlmachine_assembly:accept_model(Shaft, Criteria, body(Shaft)),
    Release = body(Shaft, Body),
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

-spec rotate_model(GearBox::assembly(), Shaft::assembly(), Motion::term()) ->
                    success(Release::assembly()) | failure(E::term(), R::term(), Rejected::assembly()).
rotate_model(_GearBox, Shaft, Motion) ->
    {ok, Body} = erlmachine_transmission:rotate_model(Shaft,  Motion, body(body)),
    Release = body(Shaft, Body),
    {ok, Release}.

-spec rotate(Part::assembly(), Motion::term()) ->
                    success(Release::assembly()) | failure(E::term(), R::term(), Rejected::assembly()).
rotate(Part, Motion) ->
    erlmachine_transmission:rotate(Part, Motion).

-spec overload_model(GearBox::assembly(), Shaft::assembly(), Load::term()) ->
                      success(Release::assembly()) | failure(E::term(), R::term(), Reject::assembly()).
overload_model(GearBox, Shaft, Load) ->
    {ok, Body} = erlmachine_system:overload_model(Shaft, Load, body(Shaft)),
    Release = body(Shaft, Body),
    erlmachine_system:overloaded(GearBox, Release, Load),
    {ok, Release}.

-spec block_model(GearBox::assembly(), Shaft::assembly(), Part::assembly(), Failure::term()) ->
                      success(Release::assembly()) | failure(E::term(), R::term(), Reject::assembly()).
block_model(GearBox, Shaft, Part, Failure) ->
    {ok, Body} = erlmachine_system:block_model(Shaft, Part, Failure, body(Shaft)),
    Release = body(Shaft, Body),
    erlmachine_system:blocked(GearBox, Release, Part, Failure),
    {ok, Release}.

-spec info(GearBox::assembly(), Shaft::assembly(), Message::term()) -> 
                  ignore.
info(_Gearbox, _Gear, _Message) -> 
    ignore.

-spec uninstall_model(GearBox::assembly(), Shaft::assembly(), Reason::term()) -> 
    ok.
uninstall_model(GearBox, Shaft, Reason) ->
    Mount = mount(Shaft),
    {ok, Body} = erlmachine_assembly:uninstall_model(Shaft, Reason, body(Shaft)),
    Release = body(Shaft, Body),
    (Mount /= undefined) andalso erlmachine_assembly:uninstalled(Mount, Reason, Release),
    (Mount == GearBox) orelse erlmachine_assembly:uninstalled(GearBox, Reason, Release),
    ok.

-spec parts(Shaft::assembly()) -> list(assembly()).
parts(Shaft) ->
    Parts = erlmachine_assembly:parts(Shaft),
    Parts.

-spec parts(Shaft::assembly(), Parts::list(assembly())) -> Release::assembly().
parts(Shaft, Parts) ->
    Release = erlmachine_assembly:parts(Shaft, Parts),
    Release.

-spec body(Shaft::assembly()) -> Body::term().
body(Shaft) ->
    Product = erlmachine_assembly:product(Shaft),
    Product#shaft.body.

-spec body(Shaft::assembly(), Body::term()) -> Release::assembly().
body(Shaft, Body) ->
    Product = erlmachine_assembly:product(Shaft),
    erlmachine_assembly:product(Shaft, Product#shaft{body=Body}).

-spec mount(Shaft::assembly()) -> Mount::assembly().
mount(Shaft) ->
    Mount = erlmachine_assembly:mount(Shaft),
    Mount.

-spec mount(Shaft::assembly(), Mount::assembly()) -> Release::assembly().
mount(Shaft, Mount) ->
    Release = erlmachine_assembly:mount(Shaft, Mount),
    Release.


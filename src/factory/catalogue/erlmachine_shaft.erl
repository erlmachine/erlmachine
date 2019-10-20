-module(erlmachine_shaft).

-export([
         install/2,
         attach/3, detach/3,
         replace/3,
         transmit/4, rotate/3, rotate/2,
         call/3, cast/3, info/3,
         accept/3,
         overload/3, block/4,
         uninstall/3
        ]).

-export([
         shaft/1,
         parts/1, parts/2, 
         body/1, body/2,
         mount/1, mount/2
        ]).

-include("erlmachine_factory.hrl").
-include("erlmachine_system.hrl").

-callback install(SN::serial_no(), MN::model_no(), PN::part_no(), Options::list()) -> 
    success(term()) | failure(term(), term(), term()) | failure(term()).

-callback replace(SN::serial_no(), ID::serial_no(), Body::term()) -> 
    success(term()) | failure(term(), term(), term()) | failure(term()).

-callback uninstall(SN::serial_no(), Reason::term(), Body::term()) -> 
    success(term()) | failure(term(), term(), term()) | failure(term()).

-callback accept(SN::serial_no(), Criteria::term(), Body::term()) -> 
    success(term(), term()) | failure(term(), term(), term()) | failure(term()).

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
    Module = model_name(Assembly), SN = serial_no(Assembly), Options = model_options(Assembly),
    %% We can check exported functions accordingly to this kind of behaviour; 
    %% We are going to add error handling later; 
    {ok, Body} = Module:install(SN, Body, Options, Env),
    %% We are going to add error handling later; 
    Release = body(Shaft, Body),
    Mount = mount(Shaft),
    (Mount /= undefined) andalso erlmachine_assembly:installed(Mount, Release),
    (Mount == GearBox) orelse erlmachine_assembly:installed(GearBox, Release),
    {ok, Release}.

-spec attach(GearBox::assembly(), Shaft::assembly(), Part::assembly()) ->
                    success(Release::assembly()) | failure(E::term(), R::term(),  Rejected::assembly()).
attach(GearBox, Shaft, Part) ->
    Parts = lists:reverse([Part|parts(Shaft)]),
    {ok, Body} = erlmachine_transmission:attach_model(Shaft, Part, body(Shaft)),
    Release = parts(body(Shaft, Body), Parts),
    erlmachine_transmission:attached(GearBox, Release, Part),
    {ok, Release}.

-spec detach(GearBox::assembly(), Shaft::assembly(), ID::serial_no()) ->
                    success(Release::assembly()) | failure(E::term(), R::term(),  Rejected::assembly()).
detach(GearBox, Shaft, ID) ->
    %% At that place we need to find Part inside assembly by SN and transmit;
    {value, Part, Parts} = lists:keytake(ID, #assembly.serial_no, parts(Shaft)),
    {ok, Body} = erlmachine_transmission:attach_model(Shaft, Part, body(Shaft)),
    Release = parts(body(Shaft, Body), Parts),
    erlmachine_transmission:attached(GearBox, Release, Part),
    {ok, Release}.

-spec replace(GearBox::assembly(), Shaft::assembly(), Repair::assembly()) ->
                     success(Release::assembly()) | failure(E::term(), R::term(), Rejected::assembly()).
replace(GearBox, Shaft, Repair) ->
    Module = model_name(Assembly), SN = serial_no(Assembly), ID = serial_no(Repair),
    Module:replace(SN, ID, Body),
    {ok, Body} = erlmachine_assembly:replace_model(Shaft, Repair, body(Shaft)),
    Release = body(Shaft, Body),
    erlmachine_assembly:replaced(GearBox, Release, Repair),
    {ok, Repair}.

%% Potentially transmit will be able to provide chained processing over list of elements;
-spec transmit(GearBox::assembly(), Shaft::assembly(), Part::assembly(), Motion::term()) ->
                      success(Result::term(), Release::assembly()) | failure(E::term(), R::term(), Rejected::assembly()).
transmit(_GearBox, Shaft, Part, Motion) ->
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

-spec accept(GearBox::assembly(), Shaft::assembly(), Criteria::term()) ->
                    success(Report::term(), Release::assembly())| failure(E::term(), R::term(), Rejected::assembly()).
accept(GearBox, Shaft, Criteria) ->
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

-spec rotate(GearBox::assembly(), Shaft::assembly(), Motion::term()) ->
                    success(Release::assembly()) | failure(E::term(), R::term(), Rejected::assembly()).
rotate(_GearBox, Shaft, Motion) ->
    {ok, Body} = erlmachine_transmission:rotate_model(Shaft,  Motion, body(body)),
    Release = body(Shaft, Body),
    {ok, Release}.

-spec rotate(Part::assembly(), Motion::term()) ->
                    success(Release::assembly()) | failure(E::term(), R::term(), Rejected::assembly()).
rotate(Part, Motion) ->
    erlmachine_transmission:rotate(Part, Motion).

-spec overload(GearBox::assembly(), Shaft::assembly(), Load::term()) ->
                      success(Release::assembly()) | failure(E::term(), R::term(), Reject::assembly()).
overload(GearBox, Shaft, Load) ->
    {ok, Body} = erlmachine_system:overload_model(Shaft, Load, body(Shaft)),
    Release = body(Shaft, Body),
    erlmachine_system:overloaded(GearBox, Release, Load),
    {ok, Release}.

-spec block(GearBox::assembly(), Shaft::assembly(), Part::assembly(), Failure::term()) ->
                      success(Release::assembly()) | failure(E::term(), R::term(), Reject::assembly()).
block(GearBox, Shaft, Part, Failure) ->
    {ok, Body} = erlmachine_system:block_model(Shaft, Part, Failure, body(Shaft)),
    Release = body(Shaft, Body),
    erlmachine_system:blocked(GearBox, Release, Part, Failure),
    {ok, Release}.

-spec info(GearBox::assembly(), Shaft::assembly(), Message::term()) -> 
                  ignore.
info(_Gearbox, _Gear, _Message) -> 
    ignore.

-spec uninstall(GearBox::assembly(), Shaft::assembly(), Reason::term()) -> 
    ok.
uninstall(GearBox, Shaft, Reason) ->
    Mount = mount(Shaft),
    {ok, Body} = erlmachine_assembly:uninstall_model(Shaft, Reason, body(Shaft)),
    Release = body(Shaft, Body),
    (Mount /= undefined) andalso erlmachine_assembly:uninstalled(Mount, Reason, Release),
    (Mount == GearBox) orelse erlmachine_assembly:uninstalled(GearBox, Reason, Release),
    ok.

%%%

-spec accept_model(Assembly::assembly(), Criteria::term(), Body::term()) ->
                          success(Report::term(), Release::term()) | failure(E::term(), R::term(), Reject::term()).
accept_model(Assembly, Criteria, Body) ->
    Module = model_name(Assembly),
    SN = serial_no(Assembly),
    Module:accept(SN, Criteria, Body).

-spec uninstall_model(Assembly::assembly(), Reason::term(), Body::term()) ->
                       ok.
uninstall_model(Assembly, Reason, Body) ->
    Module = model_name(Assembly),
    SN = serial_no(Assembly),
    Module:uninstall(SN, Reason, Body).

-spec installed(Assembly::assembly(), Part::assembly()) ->
                       ok.
installed(Assembly, Part) ->
    Module = prototype_name(Assembly),
    SN = serial_no(Assembly),
    Module:installed(SN, Assembly, Part).

-spec replaced(Assembly::assembly(), Part::assembly(), Extension::assembly()) ->
                     ok.
replaced(Assembly, Part, Extension) ->
    Module = prototype_name(Assembly),
    SN = serial_no(Assembly),
    Module:replaced(SN, Assembly, Part, Extension).

-spec accepted(Assembly::assembly(), Part::assembly(), Criteria::term(), Report::term()) ->
                      ok.
accepted(Assembly, Part, Criteria, Report) ->
    Module = prototype_name(Assembly),
    SN = serial_no(Assembly),
    Module:accepted(SN, Assembly, Part, Criteria, Report).

-spec rejected(Assembly::assembly(), Part::assembly(), Criteria::term(), Report::term()) ->
                      ok.
rejected(Assembly, Part, Criteria, Report) ->
    Module = prototype_name(Assembly),
    SN = serial_no(Assembly),
    Module:rejected(SN, Assembly, Part, Criteria, Report).

-spec uninstalled(Assembly::assembly(), Reason::term(), Part::assembly()) ->
                       ok.
uninstalled(Assembly, Reason, Part) ->
    Module = prototype_name(Assembly),
    SN = serial_no(Assembly),
    Module:uninstalled(SN, Assembly, Part, Reason).

%%

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


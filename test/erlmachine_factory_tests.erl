-module(erlmachine_factory_tests).
%% NOTE: The factory test is responsible to inspect:
%% a) Release of default extensions: gear, shaft, axle, gearbox;
%% b) Release of an extension via datasheet;
%% c) Release of a transmission via datasheet
-include_lib("eunit/include/eunit.hrl").

erlmachine_factory_test_() ->
    {
     foreach,
     fun() ->
             application:start(yamerl),

             mnesia:create_schema([node()]), ok = mnesia:start(),
             erlmachine_app:wait_for_tables(1000),
             {ok, _} = erlmachine_factory:start(),

             [ok = erlmachine_app:add_schema(File) || File <- ["assembly.json", "transmission.json"]]
     end,
     fun(_) ->
             mnesia:stop(),
             erlmachine_factory:stop(),
             application:stop(yamerl)
     end,
     [
      {
       "Inspect assembly of worker type",
       fun() ->
               Path = filename:join(erlmachine:priv_dir(), "datasheets/sample.yaml"),
               {ok, Datasheet} = erlmachine_datasheet:assembly(Path),

               Assembly = erlmachine_factory:assembly(Datasheet),
               true = erlmachine:is_worker(Assembly)
       end
      },
      {
       "Inspect assembly of supervisor type",
       fun() ->
               Path = filename:join(erlmachine:priv_dir(), "datasheets/sup_sample.yaml"),
               {ok, Datasheet} = erlmachine_datasheet:assembly(Path),

               Assembly = erlmachine_factory:assembly(Datasheet),
               true = erlmachine:is_supervisor(Assembly)
       end
      },
      {
        "Inspect gear",
       fun() ->
               Gear = erlmachine_factory:gear(erlmachine_model_ct, [], ['eunit']),
               <<"GR-", _/binary>> = erlmachine_assembly:serial_no(Gear)
       end
      },
      {
       "Inspect shaft",
        fun() ->
                Shaft = erlmachine_factory:shaft(erlmachine_model_ct, [], ['eunit'], []),
                <<"ST-", _/binary>> = erlmachine_assembly:serial_no(Shaft)
        end
      },
      {
       "Inspect axle",
       fun() ->
               Axle = erlmachine_factory:axle(erlmachine_sup_model_ct, [], ['eunit'], []),
               <<"AE-", _/binary>> = erlmachine_assembly:serial_no(Axle)
       end
      },
      {
       "Inspect gearbox",
       fun() ->
               GearBox = erlmachine_factory:gearbox(erlmachine_sup_model_ct, [], #{}, ['eunit'], []),
               <<"GX-", _/binary>> = erlmachine_assembly:serial_no(GearBox)
       end
      },
      {
       "Inspect datasheet mapping (sample.yaml)",
       fun() ->
               Path = filename:join(erlmachine:priv_dir(), "datasheets/sample.yaml"),
               {ok, Datasheet} = erlmachine_datasheet:assembly(Path),

               Assembly = erlmachine_factory:assembly(Datasheet),
               Model = erlmachine_assembly:model(Assembly),
               Prot = erlmachine_assembly:prototype(Assembly),

               <<"SN-0">> = erlmachine_assembly:serial_no(Assembly),

               true = erlmachine:is_worker(Assembly),

               true = is_map(erlmachine_assembly:body(Assembly)),

               <<"MN-0">> = erlmachine_assembly:model_no(Assembly),

               <<"#">> = erlmachine_assembly:socket(Assembly),

               'erlmachine_model_ct' = erlmachine_model:module(Model),

               true = is_list(erlmachine_model:options(Model)),

               <<"0.0.1">> = erlmachine_model:vsn(Model),

               'erlmachine_prototype_def' = erlmachine_prototype:module(Prot),

               true = is_list(erlmachine_prototype:options(Prot)),

               1 = erlmachine_prototype:vsn(Prot),

               true = is_integer(erlmachine_assembly:uid(Assembly)),

               true = is_list(erlmachine_assembly:tags(Assembly)),

               <<"vertex-test">> = erlmachine_assembly:vertex(Assembly),

               <<"PN-0">> = erlmachine_assembly:part_no(Assembly),

               true = is_map(erlmachine_assembly:env(Assembly)),

               true = is_binary(erlmachine_assembly:description(Assembly))
       end
      },
      {
       "Inspect transmission mapping (transmission_sample.yaml)",
       fun() ->
               ok
       end
      }
      {
       "Inspect serial rotation",
       fun() ->
               ok
       end
      }
     ]
    }.

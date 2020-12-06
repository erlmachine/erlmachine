-module(erlmachine_factory_tests).

-include_lib("eunit/include/eunit.hrl").

erlmachine_factory_test_() ->
    {
     foreach,
     fun() ->
             ok = application:start(yamerl),

             mnesia:create_schema([node()]), ok = mnesia:start(),
             ok = mnesia:wait_for_tables([erlmachine_factory:tabname()], 1000),
             {ok, _} = erlmachine_factory:start(),

             Priv = erlmachine:priv_dir(),
             [Schema] = jsx:consult(filename:join(Priv, "datasheet.json"), [return_maps]),
             Key = erlmachine_datasheet:schema(),
             ok = jesse:add_schema(Key, Schema)
     end,
     fun(_) ->
             mnesia:stop(),
             ok = erlmachine_factory:stop(),
             ok = application:stop(yamerl)
     end,
     [
      {
        "Gear (default prototype)",
       fun() ->
               Gear = erlmachine_factory:gear(erlmachine_gear_sample, []),
               SN = erlmachine_assembly:serial_no(Gear), true = is_binary(SN),
               ?debugFmt("Gear (default prototype): ~p", [SN])
       end
      },
      {
        "Shaft (default prototype)",
        fun() ->
                Shaft = erlmachine_factory:shaft(erlmachine_shaft_sample, [], []),
                SN = erlmachine_assembly:serial_no(Shaft), true = is_binary(SN),
                ?debugFmt("Shaft (default prototype): ~p", [SN])
        end
      },
      {
       "Axle (default prototype)",
       fun() ->
               Axle = erlmachine_factory:axle(erlmachine_axle_sample, [], []),
               SN = erlmachine_assembly:serial_no(Axle), true = is_binary(SN),
               ?debugFmt("Axle (default prototype): ~p", [SN])
       end
      },
      {
       "GearBox (default prototype)",
       fun() ->
               Gearbox = erlmachine_factory:gearbox(erlmachine_gearbox_sample, [], #{}, []),
               SN = erlmachine_assembly:serial_no(Gearbox), true = is_binary(SN),
               ?debugFmt("GearBox (default prototype): ~p", [SN])
       end
      },
      {
       "Gear (datasheet)",
       fun() ->
               Path = filename:join(erlmachine:priv_dir(), "datasheets/worker_sample.yaml"),

               {ok, Datasheet} = erlmachine_datasheet:file(Path),
               Gear = erlmachine_factory:gear(Datasheet),
               SN = erlmachine_assembly:serial_no(Gear), true = is_binary(SN)
       end
      },
      {
       "Shaft (datasheet)",
       fun() ->
               Path = filename:join(erlmachine:priv_dir(), "datasheets/worker_sample.yaml"),

               {ok, Datasheet} = erlmachine_datasheet:file(Path),
               Gear = erlmachine_factory:shaft(Datasheet),
               SN = erlmachine_assembly:serial_no(Gear), true = is_binary(SN)
       end
      },
      {
       "Axle (datasheet)",
       fun() ->
               Path = filename:join(erlmachine:priv_dir(), "datasheets/supervisor_sample.yaml"),

               {ok, Datasheet} = erlmachine_datasheet:file(Path),
               Gear = erlmachine_factory:axle(Datasheet),
               SN = erlmachine_assembly:serial_no(Gear), true = is_binary(SN)
       end
      },
      {
       "GearBox (datasheet)",
       fun() ->
               Path = filename:join(erlmachine:priv_dir(), "datasheets/supervisor_sample.yaml"),

               {ok, Datasheet} = erlmachine_datasheet:file(Path),
               Gear = erlmachine_factory:gearbox(Datasheet),
               SN = erlmachine_assembly:serial_no(Gear), true = is_binary(SN)
       end
      }
     ]
    }.

-module(erlmachine_factory_tests).

-include_lib("eunit/include/eunit.hrl").

erlmachine_factory_test_() ->
    {
     foreach,
     fun() ->
             application:start(yamerl),

             mnesia:create_schema([node()]), ok = mnesia:start(),
             ok = mnesia:wait_for_tables([erlmachine_factory:tabname()], 1000),
             {ok, _} = erlmachine_factory:start(),

             Path = filename:join(erlmachine:priv_dir(), "datasheet.json"),
             [Schema] = jsx:consult(Path, [return_maps]),

             ok = jesse:add_schema(_Key = erlmachine_datasheet:schema(), Schema)
     end,
     fun(_) ->
             mnesia:stop(),
             erlmachine_factory:stop(),
             application:stop(yamerl)
     end,
     [
      {
        "Inspect gear",
       fun() ->
               Gear = erlmachine_factory:gear(erlmachine_gear_sample, [], <<"Inspected gear">>),
               SN = erlmachine_assembly:serial_no(Gear), true = is_binary(SN),

               Path = filename:join(erlmachine:priv_dir(), "datasheets/worker_sample.yaml"),
               {ok, Datasheet} = erlmachine_datasheet:file(Path),

               Gear1 = erlmachine_factory:gear(Datasheet),

               {ok, SN1} = erlmachine_datasheet:find(<<"serial_no">>, Datasheet),
               SN1 = erlmachine_assembly:serial_no(Gear1), true = is_binary(SN1)
       end
      },
      {
       "Inspect shaft",
        fun() ->
                Shaft0 = erlmachine_factory:shaft(erlmachine_shaft_sample, [], <<"Inspected shaft">>, []),
                SN0 = erlmachine_assembly:serial_no(Shaft0), true = is_binary(SN0),

                Path = filename:join(erlmachine:priv_dir(), "datasheets/worker_sample.yaml"),
                {ok, Datasheet} = erlmachine_datasheet:file(Path),

                Shaft1 = erlmachine_factory:shaft(Datasheet, []),
                SN1 = erlmachine_assembly:serial_no(Shaft1), true = is_binary(SN1)

        end
      },
      {
       "Inspect axle",
       fun() ->
               Axle0 = erlmachine_factory:axle(erlmachine_axle_sample, [], <<"Inspected axle">>, []),
               SN0 = erlmachine_assembly:serial_no(Axle0), true = is_binary(SN0),

               Path = filename:join(erlmachine:priv_dir(), "datasheets/supervisor_sample.yaml"),
               {ok, Datasheet} = erlmachine_datasheet:file(Path),

               Axle1 = erlmachine_factory:axle(Datasheet, []),
               SN1 = erlmachine_assembly:serial_no(Axle1), true = is_binary(SN1)

       end
      },
      {
       "Inspect gearbox",
       fun() ->
               GearBox0 = erlmachine_factory:gearbox(erlmachine_gearbox_sample, [], #{}, <<"Inspected gearbox">>, []),
               SN0 = erlmachine_assembly:serial_no(GearBox0), true = is_binary(SN0),

               Path = filename:join(erlmachine:priv_dir(), "datasheets/supervisor_sample.yaml"),
               {ok, Datasheet} = erlmachine_datasheet:file(Path),

               GearBox1 = erlmachine_factory:gearbox(Datasheet, []),
               SN1 = erlmachine_assembly:serial_no(GearBox1), true = is_binary(SN1)

       end
      },
      {
       "Inspect datasheet processing",
       fun() ->
               Path = filename:join(erlmachine:priv_dir(), "datasheets/worker_sample.yaml"),
               {ok, Datasheet} = erlmachine_datasheet:file(Path),

               Gear = erlmachine_factory:gear(Datasheet),

               {ok, SN} = erlmachine_datasheet:find(<<"serial_no">>, Datasheet),
               SN = erlmachine_assembly:serial_no(Gear),

               {ok, Body} = erlmachine_datasheet:find(<<"body">>, Datasheet),
               Body = erlmachine_assembly:body(Gear),

               {ok, MN} = erlmachine_datasheet:find(<<"model_no">>, Datasheet),
               MN = erlmachine_assembly:model_no(Gear),

               {ok, Socket} = erlmachine_datasheet:find(<<"socket">>, Datasheet),
               Socket = erlmachine_assembly:socket(Gear),

               {ok, Model} = erlmachine_datasheet:find(<<"model">>, Datasheet),
               {ok, ModelName} = erlmachine_datasheet:find(<<"name">>, Model),
               {ok, ModelOpt} = erlmachine_datasheet:find(<<"options">>, Model),

               ModelNameAsAtom = binary_to_atom(ModelName, utf8),
               ModelNameAsAtom = erlmachine_model:name(erlmachine_assembly:model(Gear)),

               ModelOpt = erlmachine_model:options(erlmachine_assembly:model(Gear)),

               {ok, Prot} = erlmachine_datasheet:find(<<"prototype">>, Datasheet),
               {ok, ProtName} = erlmachine_datasheet:find(<<"name">>, Prot),
               {ok, ProtOpt} = erlmachine_datasheet:find(<<"options">>, Prot),

               ProtNameAsAtom = binary_to_atom(ProtName, utf8),
               ProtNameAsAtom = erlmachine_prototype:name(erlmachine_assembly:prototype(Gear)),

               ProtOpt = erlmachine_prototype:options(erlmachine_assembly:prototype(Gear)),

               {ok, Tags} = erlmachine_datasheet:find(<<"tags">>, Datasheet),
               Tags = erlmachine_assembly:tags(Gear),

               {ok, Label} = erlmachine_datasheet:find(<<"label">>, Datasheet),
               Label = erlmachine_assembly:label(Gear),

               {ok, PN} = erlmachine_datasheet:find(<<"part_no">>, Datasheet),
               PN = erlmachine_assembly:part_no(Gear),

               {ok, Env} = erlmachine_datasheet:find(<<"env">>, Datasheet),
               Env = erlmachine_assembly:env(Gear),

               {ok, Desc} = erlmachine_datasheet:find(<<"desc">>, Datasheet),
               Desc = erlmachine_assembly:desc(Gear)
       end
      },
      {
       "Inspect serial rotation",
       fun() ->
               ok
       end
      }
     ]
    }.

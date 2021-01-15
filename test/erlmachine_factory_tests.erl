-module(erlmachine_factory_tests).
%% NOTE: The factory test is responsible to inspect the next related concerns: gear, shaft, axle, gearbox, datasheet mapping;
-include_lib("eunit/include/eunit.hrl").

erlmachine_factory_test_() ->
    {
     foreach,
     fun() ->
             application:start(yamerl),

             mnesia:create_schema([node()]), ok = mnesia:start(),
             erlmachine_app:wait_for_tables(1000),
             {ok, _} = erlmachine_factory:start(),

             [ok = erlmachine_app:add_schema(File) || File <- ["assembly.json", "schema.json"]]
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
               Gear = erlmachine_factory:gear(erlmachine_model_ct, [], ['eunit']),
               SN = erlmachine_assembly:serial_no(Gear), true = is_binary(SN),

               Path = filename:join(erlmachine:priv_dir(), "datasheets/sample.yaml"),
               {ok, Datasheet} = erlmachine_datasheet:assembly(Path),

               Gear1 = erlmachine_factory:gear(Datasheet),

               {ok, SN1} = erlmachine_datasheet:find(<<"serial_no">>, Datasheet),
               SN1 = erlmachine_assembly:serial_no(Gear1), true = is_binary(SN1)
       end
      },
      {
       "Inspect shaft",
        fun() ->
                Shaft0 = erlmachine_factory:shaft(erlmachine_model_ct, [], ['eunit'], []),
                SN0 = erlmachine_assembly:serial_no(Shaft0), true = is_binary(SN0),

                Path = filename:join(erlmachine:priv_dir(), "datasheets/sample.yaml"),
                {ok, Datasheet} = erlmachine_datasheet:assembly(Path),

                Shaft1 = erlmachine_factory:shaft(Datasheet, []),
                SN1 = erlmachine_assembly:serial_no(Shaft1), true = is_binary(SN1)

        end
      },
      {
       "Inspect axle",
       fun() ->
               Axle0 = erlmachine_factory:axle(erlmachine_sup_model_ct, [], ['eunit'], []),
               SN0 = erlmachine_assembly:serial_no(Axle0), true = is_binary(SN0),

               Path = filename:join(erlmachine:priv_dir(), "datasheets/sup_sample.yaml"),
               {ok, Datasheet} = erlmachine_datasheet:assembly(Path),

               Axle1 = erlmachine_factory:axle(Datasheet, []),
               SN1 = erlmachine_assembly:serial_no(Axle1), true = is_binary(SN1)

       end
      },
      {
       "Inspect gearbox",
       fun() ->
               GearBox0 = erlmachine_factory:gearbox(erlmachine_sup_model_ct, [], #{}, ['eunit'], []),
               SN0 = erlmachine_assembly:serial_no(GearBox0), true = is_binary(SN0),

               Path = filename:join(erlmachine:priv_dir(), "datasheets/sup_sample.yaml"),
               {ok, Datasheet} = erlmachine_datasheet:assembly(Path),

               GearBox1 = erlmachine_factory:gearbox(Datasheet, []),
               SN1 = erlmachine_assembly:serial_no(GearBox1), true = is_binary(SN1)

       end
      },
      {
       "Inspect datasheet processing",
       fun() ->
               Path = filename:join(erlmachine:priv_dir(), "datasheets/sample.yaml"),
               {ok, Datasheet} = erlmachine_datasheet:assembly(Path),

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

               {ok, UID} = erlmachine_datasheet:find(<<"uid">>, Datasheet),
               UID = erlmachine_assembly:uid(Gear),

               {ok, Tags} = erlmachine_datasheet:find(<<"tags">>, Datasheet),
               Tags = erlmachine_assembly:tags(Gear),

               {ok, V} = erlmachine_datasheet:find(<<"vertex">>, Datasheet),
               V = erlmachine_assembly:vertex(Gear),

               {ok, PN} = erlmachine_datasheet:find(<<"part_no">>, Datasheet),
               PN = erlmachine_assembly:part_no(Gear),

               {ok, Env} = erlmachine_datasheet:find(<<"env">>, Datasheet),
               Env = erlmachine_assembly:env(Gear),

               {ok, Desc} = erlmachine_datasheet:find(<<"description">>, Datasheet),
               Desc = erlmachine_assembly:description(Gear)
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

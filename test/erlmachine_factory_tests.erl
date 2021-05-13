-module(erlmachine_factory_tests).
%% NOTE: The factory test is responsible to inspect:
%% a) Release of predefined extensions: gear, shaft, axle, gearbox;
%% b) Release of an assembly via datasheet;
%% c) Release of a transmission via datasheet
-include_lib("eunit/include/eunit.hrl").

erlmachine_factory_test_() ->
    Nodes = [node()], Table = 'erlmachine_factory',

    {
     foreach,
     fun() ->
             meck:expect(erlmachine, get_key, 1, {ok, ['erlmachine_assembly']}),

             erlmachine_database:create_schema(Nodes), ok = erlmachine_database:start(),

             erlmachine_database:create_table(Table),
             erlmachine_app:wait_for_tables([Table], 1000),

             application:start(yamerl), application:start(syn),
             {ok, _} = erlmachine_factory:start(),

             [ok = erlmachine_app:add_schema(File) || File <- ["assembly.json", "graph.json"]]
     end,
     fun(_) ->
             erlmachine_factory:stop(),
             application:stop(yamerl), application:start(syn),
             erlmachine_database:delete_table(Table), erlmachine_database:delete_schema(Nodes)
     end,
     [
      {
       "Inspect assembly: datasheets/extensions/ct.yaml",
       fun() ->
               Path = filename:join(erlmachine:priv_dir(), "datasheets/extensions/ct.yaml"),
               {ok, Datasheet} = erlmachine_datasheet:assembly(Path),

               Assembly = erlmachine_factory:assembly(Datasheet),

               SN = erlmachine:serial_no(Assembly), true = is_binary(SN)
       end
      },
      {
       "Inspect assembly: datasheets/extensions/sup_ct.yaml",
       fun() ->
               Path = filename:join(erlmachine:priv_dir(), "datasheets/extensions/sup_ct.yaml"),
               {ok, Datasheet} = erlmachine_datasheet:assembly(Path),

               Assembly = erlmachine_factory:assembly(Datasheet),

               SN = erlmachine:serial_no(Assembly), true = is_binary(SN)
       end
      },
      {
       "Inspect assembly: gear",
       fun() ->
               Gear = erlmachine_factory:gear(erlmachine_model_ct, [], ['eunit']),

               SN = erlmachine:serial_no(Gear), V = erlmachine:vertex(Gear),
               true = is_binary(SN), SN = V
       end
      },
      {
       "Inspect assembly: shaft",
        fun() ->
                Shaft = erlmachine_factory:shaft(erlmachine_model_ct, [], ['eunit'], []),

                SN = erlmachine:serial_no(Shaft), V = erlmachine:vertex(Shaft),
                true = is_binary(SN), SN = V
        end
      },
      {
       "Inspect assembly: axle",
       fun() ->
               Axle = erlmachine_factory:axle(erlmachine_sup_model_ct, [], ['eunit'], []),

               SN = erlmachine:serial_no(Axle), V = erlmachine:vertex(Axle),
               true = is_binary(SN), SN = V
       end
      },
      {
       "Inspect assembly: gearbox",
       fun() ->
               GearBox = erlmachine_factory:gearbox(erlmachine_sup_model_ct, [], ['eunit'], []),

               SN = erlmachine:serial_no(GearBox), V = erlmachine:vertex(GearBox),
               true = is_binary(SN), SN = V
       end
      },
      {
       "Inspect datasheet mapping: datasheets/extensions/ct.yaml",
       fun() ->
               Path = filename:join(erlmachine:priv_dir(), "datasheets/extensions/ct.yaml"),
               {ok, Datasheet} = erlmachine_datasheet:assembly(Path),

               Assembly = erlmachine_factory:assembly(Datasheet),
               Model = erlmachine_assembly:model(Assembly), Prot = erlmachine_assembly:prototype(Assembly),

               SN = erlmachine:serial_no(Assembly), true = is_binary(SN),
 
               true = erlmachine:is_worker(Assembly),

               Body = erlmachine_assembly:body(Assembly), true = is_map(Body),

               MN = erlmachine_assembly:model_no(Assembly), true = is_binary(MN),

               Port = erlmachine:port(Assembly), true = is_binary(Port),

               Module = erlmachine_model:module(Model), 'erlmachine_model_ct' = Module,

               Opt = erlmachine_model:options(Model), true = is_list(Opt),

               Module2 = erlmachine_prototype:module(Prot), 'erlmachine_prototype_def' = Module2,

               Opt2 = erlmachine_prototype:options(Prot), true = is_list(Opt2),

               UID = erlmachine:uid(Assembly), true = is_integer(UID),

               Tags = erlmachine:tags(Assembly), true = is_list(Tags),

               Vertex = erlmachine:vertex(Assembly), true = is_binary(Vertex),

               PN = erlmachine:part_no(Assembly), true = is_binary(PN),

               Env = erlmachine_assembly:env(Assembly), true = is_map(Env),

               Desc = erlmachine:description(Assembly), true = is_binary(Desc)
       end
      },
      {
       "Inspect datasheet mapping: datasheets/ct.yaml",
       fun() ->
               Path = filename:join(erlmachine:priv_dir(), "datasheets/ct.yaml"),
               {ok, Datasheet} = erlmachine_datasheet:graph(Path),

               Graph = erlmachine_factory:graph(Datasheet),

               Exts = erlmachine_graph:vertices(Graph), [_A, _B, _C, _D, _E] = Exts,
               [true = is_binary(erlmachine:serial_no(Ext)) || Ext <- Exts]
       end
      },
      {
       "Inspect serial: rotation",
       fun() ->
               ok
       end
      }
     ]
    }.


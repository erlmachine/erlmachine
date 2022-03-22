-module(erlmachine_factory_tests).

-include_lib("eunit/include/eunit.hrl").

erlmachine_factory_test_() ->
    Modules = [ 'erlmachine_factory', 'erlmachine_assembly', 'erlmachine_graph',
                'erlmachine_system'
              ],

    Nodes = [node()],
    Tables = ['erlmachine_factory'],

    { foreach,
      fun() ->
              mnesia:create_schema(Nodes),
              mnesia:start(),

              application:start(yamerl),
              application:start(syn),

              meck:expect(erlmachine, modules, 0, Modules),

              ok = erlmachine:init(Modules),
              ok = mnesia:wait_for_tables(['erlmachine_factory'], 1000),

              {ok, _} = erlmachine_factory:start()
      end,
      fun(_) ->
              ok = erlmachine_factory:stop(),

              ok = application:stop(yamerl),
              ok = application:stop(syn),
              ok
      end,
      [
       { "Inspect assembly: datasheets/extensions/ct.yaml",
         fun() ->
                 Path = erlmachine:filename("datasheets/extensions/ct.yaml"),
                 {ok, T} = erlmachine_assembly:template(Path),

                 Assembly = erlmachine_factory:assembly(T),

                 SN = erlmachine:serial_no(Assembly), true = is_binary(SN)
         end
       },
       { "Inspect assembly: datasheets/extensions/sup_ct.yaml",
         fun() ->
                 Path = erlmachine:filename("datasheets/extensions/sup_ct.yaml"),
                 {ok, T} = erlmachine_assembly:template(Path),

                 Assembly = erlmachine_factory:assembly(T),

                 SN = erlmachine:serial_no(Assembly), true = is_binary(SN)
         end
       },
       { "Inspect assembly: gear",
         fun() ->
                 Gear0 = erlmachine_factory:gear('erlmachine_model_ct', #{}, _Env = #{}),
                 Gear1 = erlmachine:tags(Gear0, _Tags = ['eunit']),

                 SN = erlmachine:serial_no(Gear1), V = erlmachine:vertex(Gear1), true = is_binary(SN),
                 SN = V
         end
      },
      { "Inspect assembly: shaft",
        fun() ->
                Shaft0 = erlmachine_factory:shaft('erlmachine_model_ct', #{}, _Env = #{}, []),
                Shaft1 = erlmachine:tags(Shaft0, _Tags = ['eunit']),

                SN = erlmachine:serial_no(Shaft1), V = erlmachine:vertex(Shaft1), true = is_binary(SN),
                SN = V
        end
      },
      { "Inspect assembly: axle",
        fun() ->
                Axle0 = erlmachine_factory:axle('erlmachine_sup_model_ct', #{}, _Env = #{}, []),
                Axle1 = erlmachine:tags(Axle0, _Tags = ['eunit']),

                SN = erlmachine:serial_no(Axle1), V = erlmachine:vertex(Axle1), true = is_binary(SN),
                SN = V
        end
      },
      { "Inspect assembly: gearbox",
        fun() ->
                GearBox0 = erlmachine_factory:gearbox('erlmachine_sup_model_ct', #{}, _Env = #{}, []),
                GearBox1 = erlmachine:tags(GearBox0, _Tags = ['eunit']),

                SN = erlmachine:serial_no(GearBox1), V = erlmachine:vertex(GearBox1), true = is_binary(SN),
                SN = V
        end
      },
      { "Inspect datasheet mapping: datasheets/extensions/ct.yaml",
        fun() ->
                Path = erlmachine:filename("datasheets/extensions/ct.yaml"),
                {ok, T} = erlmachine_assembly:template(Path),

                Assembly = erlmachine_factory:assembly(T),

                SN = erlmachine:serial_no(Assembly),

                true = is_binary(SN),
                true = erlmachine:is_worker(Assembly),

                Body = erlmachine_assembly:body(Assembly),
                true = is_map(Body),

                MN = erlmachine_assembly:model_no(Assembly),
                true = is_binary(MN),

                Port = erlmachine:port(Assembly),
                true = is_binary(Port),

                Model = erlmachine_assembly:model(Assembly),
                'erlmachine_model_ct' = erlmachine_model:module(Model),

                Opt = erlmachine_model:options(Model),
                true = is_map(Opt),

                Prot = erlmachine_assembly:prototype(Assembly),
                'erlmachine_prototype_def' = erlmachine_prototype:module(Prot),

                Opt2 = erlmachine_prototype:options(Prot),
                true = is_map(Opt2),

                UID = erlmachine:uid(Assembly),
                true = is_integer(UID),

                Tags = erlmachine:tags(Assembly),
                true = is_list(Tags),

                Vertex = erlmachine:vertex(Assembly),
                true = is_binary(Vertex),

                PN = erlmachine:part_no(Assembly),
                true = is_binary(PN),

                Env = erlmachine_assembly:env(Assembly),
                true = is_map(Env),

                Desc = erlmachine:description(Assembly),
                true = is_binary(Desc)
        end
      },
      { "Inspect datasheet mapping: datasheets/ct.yaml",
        fun() ->
                {ok, T} = erlmachine_graph:template(_Path = erlmachine:filename("datasheets/ct.yaml")),

                Graph = erlmachine_factory:graph(T),

                Exts = erlmachine_graph:vertices(Graph),
                [_A, _B, _C, _D, _E] = Exts,

                [ begin SN = erlmachine:serial_no(Ext),
                        true = is_binary(SN) end || Ext <- Exts ]
        end
      },
       { "Inspect serial: rotation",
        fun() -> ok
        end
       }
      ]
    }.

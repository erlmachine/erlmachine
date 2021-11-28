-module(erlmachine_factory_tests).

-include_lib("eunit/include/eunit.hrl").

erlmachine_factory_test_() ->
    Modules = ['erlmachine_factory', 'erlmachine_assembly', 'erlmachine_graph', 'erlmachine_system'],

    Nodes = [node()],

    Tables = ['erlmachine_factory'],
    Tags = ['eunit'],
    {
     foreach,
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
      {
       "Inspect assembly: datasheets/extensions/ct.yaml",
       fun() ->
               Name = erlmachine:filename("datasheets/extensions/ct.yaml"),
               {ok, T} = erlmachine_assembly:template(Name),

               Assembly = erlmachine_factory:assembly(T),

               SN = erlmachine:serial_no(Assembly), true = is_binary(SN)
       end
      },
      {
       "Inspect assembly: datasheets/extensions/sup_ct.yaml",
       fun() ->
               Name = erlmachine:filename("datasheets/extensions/sup_ct.yaml"),
               {ok, T} = erlmachine_assembly:template(Name),

               Assembly = erlmachine_factory:assembly(T),

               SN = erlmachine:serial_no(Assembly), true = is_binary(SN)
       end
      },
      {
       "Inspect assembly: gear",
       fun() ->
               Gear = erlmachine:tags(erlmachine_factory:gear('erlmachine_model_ct', _Opt = #{}, _Env = #{}), Tags),

               SN = erlmachine:serial_no(Gear), V = erlmachine:vertex(Gear),
               true = is_binary(SN), SN = V
       end
      },
      {
       "Inspect assembly: shaft",
        fun() ->
                Shaft = erlmachine:tags(erlmachine_factory:shaft('erlmachine_model_ct', _Opt = #{}, _Env = #{}, []), Tags),

                SN = erlmachine:serial_no(Shaft), V = erlmachine:vertex(Shaft),
                true = is_binary(SN), SN = V
        end
      },
      {
       "Inspect assembly: axle",
       fun() ->
               Axle = erlmachine:tags(erlmachine_factory:axle('erlmachine_sup_model_ct', _Opt = #{}, _Env = #{}, []), Tags),

               SN = erlmachine:serial_no(Axle), V = erlmachine:vertex(Axle),
               true = is_binary(SN), SN = V
       end
      },
      {
       "Inspect assembly: gearbox",
       fun() ->
               GearBox = erlmachine:tags(erlmachine_factory:gearbox('erlmachine_sup_model_ct', _Opt = #{}, _Env = #{}, []), Tags),

               SN = erlmachine:serial_no(GearBox), V = erlmachine:vertex(GearBox),
               true = is_binary(SN), SN = V
       end
      },
      {
       "Inspect datasheet mapping: datasheets/extensions/ct.yaml",
       fun() ->
               Name = erlmachine:filename("datasheets/extensions/ct.yaml"),
               {ok, T} = erlmachine_assembly:template(Name),

               Assembly = erlmachine_factory:assembly(T),
               Model = erlmachine_assembly:model(Assembly), Prot = erlmachine_assembly:prototype(Assembly),

               SN = erlmachine:serial_no(Assembly), true = is_binary(SN),

               true = erlmachine:is_worker(Assembly),

               Body = erlmachine_assembly:body(Assembly), true = is_map(Body),

               MN = erlmachine_assembly:model_no(Assembly), true = is_binary(MN),

               Port = erlmachine:port(Assembly), true = is_binary(Port),

               Module = erlmachine_model:module(Model), 'erlmachine_model_ct' = Module,

               Opt = erlmachine_model:options(Model), true = is_map(Opt),

               Module2 = erlmachine_prototype:module(Prot), 'erlmachine_prototype_def' = Module2,

               Opt2 = erlmachine_prototype:options(Prot), true = is_map(Opt2),

               UID = erlmachine:uid(Assembly), true = is_integer(UID),

               true = is_list(erlmachine:tags(Assembly)),

               Vertex = erlmachine:vertex(Assembly), true = is_binary(Vertex),

               PN = erlmachine:part_no(Assembly), true = is_binary(PN),

               Env = erlmachine_assembly:env(Assembly), true = is_map(Env),

               Desc = erlmachine:description(Assembly), true = is_binary(Desc)
       end
      },
      {
       "Inspect datasheet mapping: datasheets/ct.yaml",
       fun() ->
               Name = erlmachine:filename("datasheets/ct.yaml"),
               {ok, T} = erlmachine_graph:template(Name),

               Graph = erlmachine_factory:graph(T),

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

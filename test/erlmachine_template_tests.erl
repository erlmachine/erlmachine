-module(erlmachine_template_tests).
%% NOTE: Template test is responsible to inspect the related concerns: load, decoding and validation

-include_lib("eunit/include/eunit.hrl").

erlmachine_template_test_() ->
    Modules = [ 'erlmachine_assembly', 'erlmachine_graph'

              ],

    { foreach,
      fun() ->
              application:start(yamerl),

              Res = erlmachine:init(Modules),
              Res
      end,
      fun(_) ->
              Res = application:stop(yamerl),
              Res
      end,
     [ { "Inspect file: extensions/ct.yaml",
         fun() ->
                 Path = erlmachine:filename("extensions/ct.yaml"),

                 {ok, T} = erlmachine_assembly:template(Path),

                 ?debugFmt("~n~p~n", [T]),

                 Res = is_map(T),
                 Res = true
         end
       },
       { "Inspect file: extensions/sup_ct.yaml",
         fun() ->
                 Path = erlmachine:filename("extensions/sup_ct.yaml"),

                 {ok, T} = erlmachine_assembly:template(Path),

                 ?debugFmt("~n~p~n", [T]),

                 Res = is_map(T),
                 Res = true
         end
       },
       { "Inspect file: datasheets/ct.yaml",
         fun() ->
                 Path = erlmachine:filename("datasheets/ct.yaml"),

                 {ok, T} = erlmachine_graph:template(Path),

                 ?debugFmt("~n~p~n", [T]),

                 Res = is_map(T),
                 Res = true
         end
       }
     ]
    }.

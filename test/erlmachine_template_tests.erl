-module(erlmachine_template_tests).

%% NOTE: Template test is responsible to inspect the next related concerns:

%% 1) Load template file;
%% 2) Decoding content of the template file;
%% 3) Validation of decoded content accordingly to the schema

-include_lib("eunit/include/eunit.hrl").

erlmachine_template_test_() ->
    {
     foreach,
     fun() ->
             Templates = ['erlmachine_assembly', 'erlmachine_graph'],
             application:start(yamerl),

             [ok = erlmachine_template:add_schema(T) || T <- Templates]
     end,
     fun(_) ->
             ok = application:stop(yamerl)
     end,
     [
      {
       "Inspect file: datasheets/extensions/ct.yaml",
       fun() ->
               Name = erlmachine:filename("datasheets/extensions/ct.yaml"),

               {ok, T} = erlmachine_assembly:template(Name), true = is_map(T),
               ?debugFmt("~n~p~n", [T])

       end
      },
      {
       "Inspect file: datasheets/extensions/sup_ct.yaml",
        fun() ->
                Name = erlmachine:filename("datasheets/extensions/sup_ct.yaml"),

                {ok, T} = erlmachine_assembly:template(Name), true = is_map(T),
                ?debugFmt("~n~p~n", [T])
        end
      },
      {
       "Inspect file: datasheets/ct.yaml",
       fun() ->
               Name = erlmachine:filename("datasheets/ct.yaml"),

               {ok, T} = erlmachine_graph:template(Name), true = is_map(T),
               ?debugFmt("~n~p~n", [T])
       end
      }
     ]
    }.

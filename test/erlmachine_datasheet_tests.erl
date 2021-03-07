-module(erlmachine_datasheet_tests).
%% NOTE: Datasheet test is responsible to inspect the next related concerns: 
%% 1) Load datasheet file;
%% 2) Decoding content of the datasheet file;
%% 3) Validation of decoded content accordingly to the schema
-include_lib("eunit/include/eunit.hrl").

erlmachine_datasheet_test_() ->
    {
     foreach,
     fun() ->
             application:start(yamerl),
             [ok = erlmachine_app:add_schema(File) || File <- ["assembly.json", "transmission.json"]]
    end,
     fun(_) ->
             ok = application:stop(yamerl)
     end,
     [
      {
        "Inspect sample.yaml",
       fun() ->
               Path = filename:join(erlmachine:priv_dir(), "datasheets/sample.yaml"),

               {ok, Datasheet} = erlmachine_datasheet:assembly(Path), true = is_map(Datasheet),
               ?debugFmt("~n~p~n", [Datasheet])

       end
      },
      {
        "Inspect sup_sample.yaml",
        fun() ->
                Path = filename:join(erlmachine:priv_dir(), "datasheets/sup_sample.yaml"),

                {ok, Datasheet} = erlmachine_datasheet:assembly(Path), true = is_map(Datasheet),
                ?debugFmt("~n~p~n", [Datasheet])
        end
      },
      {
       "Inspect transmission_sample.yaml",
       fun() ->
               Path = filename:join(erlmachine:priv_dir(), "datasheets/transmission_sample.yaml"),

               {ok, Datasheet} = erlmachine_datasheet:transmission(Path), true = is_map(Datasheet),
               ?debugFmt("~n~p~n", [Datasheet])
       end
      }
     ]
    }.

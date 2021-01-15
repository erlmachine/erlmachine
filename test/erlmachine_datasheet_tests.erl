-module(erlmachine_datasheet_tests).
%% NOTE: Datasheet test is responsible to inspect the next related concerns: load, read, validation.
-include_lib("eunit/include/eunit.hrl").

erlmachine_datasheet_test_() ->
    {
     foreach,
     fun() ->
             application:start(yamerl),
             [ok = erlmachine_app:add_schema(File) || File <- ["assembly.json", "schema.json"]]
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
       "Inspect schema_sample.yaml",
       fun() ->
               Path = filename:join(erlmachine:priv_dir(), "datasheets/schema_sample.yaml"),

               {ok, Datasheet} = erlmachine_datasheet:schema(Path), true = is_map(Datasheet),
               ?debugFmt("~n~p~n", [Datasheet])
       end
      }
     ]
    }.

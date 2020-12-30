-module(erlmachine_datasheet_tests).
%% NOTE: Datasheet test is responsible to inspect the next related concerns: load, read, validation.
-include_lib("eunit/include/eunit.hrl").

erlmachine_datasheet_test_() ->
    {
     foreach,
     fun() ->
             application:start(yamerl),

             Path = filename:join(erlmachine:priv_dir(), "datasheet.json"),
             [Schema] = jsx:consult(Path, [return_maps]),

             ok = jesse:add_schema(_Key = erlmachine_datasheet:schema(), Schema)
     end,
     fun(_) ->
             ok = application:stop(yamerl)
     end,
     [
      {
        "Inspect a worker datasheet",
       fun() ->
               Path = filename:join(erlmachine:priv_dir(), "datasheets/worker_sample.yaml"),

               {ok, Datasheet} = erlmachine_datasheet:file(Path), true = is_map(Datasheet)

       end
      },
      {
        "Inspect a supervisor datasheet",
        fun() ->
                Path = filename:join(erlmachine:priv_dir(), "datasheets/supervisor_sample.yaml"),

                {ok, Datasheet} = erlmachine_datasheet:file(Path), true = is_map(Datasheet)
        end
      }
     ]
    }.

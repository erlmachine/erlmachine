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
             [ok = erlmachine_app:add_schema(File) || File <- ["assembly.json", "graph.json"]]
    end,
     fun(_) ->
             ok = application:stop(yamerl)
     end,
     [
      {
       "Inspect datasheet: datasheets/extensions/ct.yaml",
       fun() ->
               FileName = filename("datasheets/extensions/ct.yaml"),

               {ok, Datasheet} = erlmachine_assembly:datasheet(FileName), true = is_map(Datasheet),
               ?debugFmt("~n~p~n", [Datasheet])

       end
      },
      {
       "Inspect datasheet: datasheets/extensions/sup_ct.yaml",
        fun() ->
                FileName = filename("datasheets/extensions/sup_ct.yaml"),

                {ok, Datasheet} = erlmachine_assembly:datasheet(FileName), true = is_map(Datasheet),
                ?debugFmt("~n~p~n", [Datasheet])
        end
      },
      {
       "Inspect datasheet: datasheets/ct.yaml",
       fun() ->
               FileName = filename("datasheets/ct.yaml"),

               {ok, Datasheet} = erlmachine_graph:datasheet(FileName), true = is_map(Datasheet),
               ?debugFmt("~n~p~n", [Datasheet])
       end
      }
     ]
    }.

-spec filename(Path::list()) -> list().
filename(Path) ->
    filename:join(erlmachine:priv_dir(), Path).

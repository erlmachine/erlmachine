-module(erlmachine_datasheet_tests).

-include_lib("eunit/include/eunit.hrl").

erlmachine_datasheet_test_() ->
    {
     foreach,
     fun() ->
             ok = application:start(yamerl),

             mnesia:create_schema([node()]), ok = mnesia:start(),
             ok = mnesia:wait_for_tables([erlmachine_factory:tabname()], 1000),
             {ok, _} = erlmachine_factory:start(),

             Priv = erlmachine:priv_dir(),
             [Schema] = jsx:consult(filename:join(Priv, "datasheet.json"), [return_maps]),
             Key = erlmachine_datasheet:schema(),
             ok = jesse:add_schema(Key, Schema)
     end,
     fun(_) ->
             mnesia:stop(),
             ok = erlmachine_factory:stop(),
             ok = application:stop(yamerl)
     end,
     [
      {
        "Load worker datasheet",
       fun() ->
               Path = filename:join(erlmachine:priv_dir(), "datasheets/worker_sample.yaml"),

               {ok, Datasheet} = erlmachine_datasheet:file(Path), true = is_map(Datasheet)

       end
      },
      {
        "Load supervisor datasheet",
        fun() ->
                Path = filename:join(erlmachine:priv_dir(), "datasheets/supervisor_sample.yaml"),

                {ok, Datasheet} = erlmachine_datasheet:file(Path), true = is_map(Datasheet)
        end
      }
     ]
    }.


datasheet(File) ->
    Priv = erlmachine:priv_dir(), Path = filename:join(Priv, File),
    Res = {ok, Datasheet} = erlmachine_datasheet:file(Path),
    ?debugFmt("~n~p~n",[Datasheet]),
    Res.

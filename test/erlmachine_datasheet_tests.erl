-module(erlmachine_datasheet_tests).

-include_lib("eunit/include/eunit.hrl").

erlmachine_datasheet_test_() ->
    {
     foreach,
     fun() ->
             ok = application:start(yamerl),
             Nodes = [node()],
             mnesia:create_schema(Nodes), ok = mnesia:start(),
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
      { "Load worker datasheet", fun() -> {ok, _} = datasheet("datasheets/worker_sample.yaml") end },
      { "Load supervisor datasheet", fun() -> {ok, _} = datasheet("datasheets/supervisor_sample.yaml") end }
     ]
    }.


datasheet(File) ->
    Priv = erlmachine:priv_dir(),
    Path = filename:join(Priv, File),
    Res = {ok, Datasheet} = erlmachine_datasheet:datasheet(Path),
    io:format(user, "~n~p~n",[Datasheet]),
    Res.

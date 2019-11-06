-module(erlmachine_handler).
-behavior(cowboy_rest).

-export([init/2]).
-export([content_types_provided/2]).
-export([to_html/2]).

init(Req, State) ->
	{cowboy_rest, Req, State}.

content_types_provided(Req, State) ->
	{[
		{{<<"text">>, <<"html">>, '*'}, to_html}
	], Req, State}.

to_html(Req, State) ->
	{<<"<html><body>This is REST!</body></html>">>, Req, State}.

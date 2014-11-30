-module(extend104_app).

-behaviour(application).

-export([start/0, stop/0]).
%% Application callbacks
-export([start/2, stop/1]).



%% ===================================================================
%% Application callbacks
%% ===================================================================

start() ->
	io:format("start extend104..."),
	[start_app(App) || App <- [crypto, lager, amqp_client, extend104] ].

start_app(App) ->
	io:format("start app:~p ~n", [App]),
    ok = application:start(App).
	
stop() ->
	[application:stop(App) || App <- lists:reverse([crypto, lager, amqp_client, extend104])].
	

start(_StartType, _StartArgs) ->
	{ok, [[CityId]]} = init:get_argument(cityid),
    extend104_sup:start_link(CityId).

stop(_State) ->
	io:format("stop extend104..."),
    ok.


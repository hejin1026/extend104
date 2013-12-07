%%%----------------------------------------------------------------------
%%% Created	: 2013-12-4
%%% author 	: hejin1026@gmail.com
%%%----------------------------------------------------------------------
-module(master_app).

-behaviour(application).

-export([start/0, stop/0]).

-export([start/2, stop/1]).


%% ===================================================================
%% Application callbacks
%% ===================================================================

start() ->
	io:format("start extend104..."),
	[start_app(App) || App <- [crypto, mnesia, lager, emysql, amqp_client, master] ].
	
start_app(App) ->
    ok = application:start(App).	
	
stop() ->
	stop_app([master, amqp_client, emysql, lager, mnesia, crypto]).

stop_app(App) ->
	ok = application:stop(App).	

start(_StartType, _StartArgs) ->
    master_sup:start_link().

stop(_State) ->
    ok.

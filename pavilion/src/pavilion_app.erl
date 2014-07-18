%%%----------------------------------------------------------------------
%%% Created	: 2014-07-11
%%% author 	: hejin1026@gmail.com
%%%----------------------------------------------------------------------
-module(pavilion_app).

-behaviour(application).

-export([start/0, stop/0]).

-export([start/2, stop/1]).

-define(APP, [crypto, lager, ranch, cowlib, cowboy, amqp_client, pavilion] ).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start() ->
	io:format("start pavilion..."),
	[start_app(App) || App <- ?APP].
	
start_app(App) ->
    ok = application:start(App).	
	
stop() ->
	[start_app(App) || App <- lists:reverse(?APP)].

stop_app(App) ->
	ok = application:stop(App).	

start(_StartType, _StartArgs) ->
    pavilion_sup:start_link().

stop(_State) ->
    ok.

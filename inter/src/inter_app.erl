
-module(inter_app).

-behaviour(application).

-export([start/0, stop/0]).

-export([start/2, stop/1]).

-define(APP, [crypto, lager, ranch, cowlib, cowboy, inter] ).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start() ->
	io:format("start inter..."),
	[start_app(App) || App <- ?APP].
	
start_app(App) ->
    ok = application:start(App).	
	
stop() ->
	[stop_app(App) || App <- lists:reverse(?APP)].

stop_app(App) ->
	ok = application:stop(App).	

start(_StartType, _StartArgs) ->
    inter_sup:start_link().

stop(_State) ->
    ok.

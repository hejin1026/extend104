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
	ok = application:start(crypto),
	ok = application:start(lager),
	ok = application:start(ranch),
	ok = application:start(cowlib),
	ok = application:start(cowboy),
	ok = application:start(extend104).
	
stop() ->
    application:stop(extend104), 
    application:stop(lager),
	application:stop(ranch),
    application:stop(crypto).	
	

start(_StartType, _StartArgs) ->
    extend104_sup:start_link().

stop(_State) ->
    ok.

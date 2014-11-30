-module(worker_pool_app).

-behavior(application).

-export([start/2, stop/1]).

start(normal, _) ->
	worker_pool_sup:start_link().

stop(_) ->
	ok.

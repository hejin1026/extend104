-module(agent_app).

-created("hejin 2012-5-3").

-include_lib("elog/include/elog.hrl").

-export([start/0, cluster/1, stop/0]).

-behavior(application).

-export([start/2, stop/1]).

start() ->
    ok = application:start(elog),
    ok = application:start(amqp_client),
    {_, OsType} = os:type(),
    [start_app(App) || App <- apps(OsType)],
    io:format("~nfinished.~n", []).


start_app(App) ->
    application:start(App).

stop() ->
    {_, OsType} = os:type(),
    [stop_app(App) || App <- lists:reverse(apps(OsType))],
    application:stop(amqp_client),
    application:stop(elog),
    io:format("~nstopped.~n", []).
    
stop_app(App) ->
    application:stop(App).

apps('hp-ux') ->
    [sasl, agent];
apps(aix) ->
    [sasl, agent];
apps(_) ->
    [sasl, os_mon, agent].

cluster(Node) ->
    case net_adm:ping(Node) of
    pang -> {error, "cannot connect to node"};
    pong -> {ok, [node() | nodes()]}
    end.

start(normal, _Args) ->
	agent_sup:start_link().

stop(_) ->
	ok.
	

%%%----------------------------------------------------------------------
%%% Author  : hejin1026@gmail.com
%%% Purpose : http send packet
%%% Created : 2013 10 28
%%%----------------------------------------------------------------------
-module(extend104_httpd).

-author('hejin').

-include_lib("elog/include/elog.hrl").

-export([start/1]).

-define(API_WSOCKET, "/websocket").

%% External API
start(Opts) ->
	Dispatch = cowboy_router:compile([
		{'_', [
			{"/", cowboy_static, [
				{directory, {priv_dir, extend104, []}},
				{file, <<"index.html">>},
				{mimetypes, [{<<".html">>, [<<"text/html">>]}]}
			]},
			{"/measure", extend104_http, []},
			{"/static/[...]", cowboy_static, [
				{directory, {priv_dir, extend104, [<<"static">>]}},
				{mimetypes, [{<<".js">>, [<<"application/javascript">>]}]}
			]},
			{"/websocket", extend104_websocket, []} 
		]}
	]),
	%% Name, NbAcceptors, TransOpts, ProtoOpts
	{ok, Pid} = cowboy:start_http(?MODULE, 100, Opts,
		[{env, [{dispatch, Dispatch}]}]
	),
	Port = proplists:get_value(port, Opts),
	?PRINT("Extend104 Httpd is listening on ~p~n", [Port]),
	{ok, Pid}.
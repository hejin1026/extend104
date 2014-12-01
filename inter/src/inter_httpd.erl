%%%----------------------------------------------------------------------
%%% Author  : hejin1026@gmail.com
%%% Purpose : http send packet
%%% Created : 2013 10 28
%%%----------------------------------------------------------------------
-module(inter_httpd).

-include_lib("elog/include/elog.hrl").

-export([start_link/1]).

-define(API_WSOCKET, "/websocket").

%% External API
start_link(Opts) ->
	App = inter,
	Dispatch = cowboy_router:compile([
		{'_', [
			{"/test_value", cowboy_static, [
				{directory, {priv_dir, App, []}},
				{file, <<"test_value.html">>},
				{mimetypes, [{<<".html">>, [<<"text/html">>]}]}
			]},
			{"/test_value.json", inter_http, []},
			{"/recv_value", inter_http, []},
			{"/static/[...]", cowboy_static, [
				{directory, {priv_dir, App, [<<"static">>]}},
				{mimetypes, [{<<".js">>, [<<"application/javascript">>]}]}
			]}
		]}
	]),
	%% Name, NbAcceptors, TransOpts, ProtoOpts
	{ok, Pid} = cowboy:start_http(?MODULE, 100, Opts,
		[{env, [{dispatch, Dispatch}]}]
	),
	Port = proplists:get_value(port, Opts),
	?PRINT("Httpd is listening on ~p~n", [Port]),
	{ok, Pid}.
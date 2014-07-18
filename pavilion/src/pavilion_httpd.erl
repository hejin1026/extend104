%%%----------------------------------------------------------------------
%%% Author  : hejin1026@gmail.com
%%% Purpose : http send packet
%%% Created : 2014 7 11
%%%----------------------------------------------------------------------
-module(pavilion_httpd).

-author('hejin 2014-7-11').

-include_lib("elog/include/elog.hrl").

-export([start/1]).

-define(API_WSOCKET, "/websocket").

%% External API
start(Opts) ->
	App = master,
	Dispatch = cowboy_router:compile([
		{'_', [
			{"/websocket", pavilion_websocket, []} 
		]}
	]),
	%% Name, NbAcceptors, TransOpts, ProtoOpts
	{ok, Pid} = cowboy:start_http(?MODULE, 100, Opts,
		[{env, [{dispatch, Dispatch}]}]
	),
	Port = proplists:get_value(port, Opts),
	?PRINT("Httpd is listening on ~p~n", [Port]),
	{ok, Pid}.
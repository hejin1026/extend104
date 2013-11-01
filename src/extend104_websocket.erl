%%%----------------------------------------------------------------------
%%% Author  : hejin1026@gmail.com
%%% Purpose : http send packet
%%% Created : 2013 10 28
%%%----------------------------------------------------------------------
-module(extend104_websocket).

-author('hejin1026@gmail.com').

-include("extend104.hrl").

-include_lib("elog/include/elog.hrl").

-behaviour(cowboy_websocket_handler).

-export([init/3,
		websocket_init/3,
		websocket_handle/3,
		websocket_info/3,
		websocket_terminate/3]).

-record(state, {conn_pid=[]}).

%Websocket long connect

init({tcp, http}, _Req, _Opts) ->
	?INFO("init get ws:~p", [self()]),
	{upgrade, protocol, cowboy_websocket}.

websocket_init(_TransportName, Req, _Opts) ->
	erlang:start_timer(2000, self(), <<"Hello!Send Req First...">>),
	{ok, Req, #state{}}.

websocket_handle({text, <<"connection:", ConnConf/binary>>}, Req, #state{conn_pid=CP} = State) ->
	try parse_conn(ConnConf) of
		{ok, Oid} ->
			case extend104:get_conn_pid(Oid) of
				{ok, ConnPid} ->
					extend104_connection:subscribe(ConnPid, self()),
					{reply, {text, << "begin to recv frame form ", ConnConf/binary >>}, Req, State#state{conn_pid = [ConnPid|CP]}};
				error ->
					{reply, {text, << "can not find connection ", ConnConf/binary >>}, Req, State}
			end
	catch _:_ERROR ->
		{reply, {text, << "connection info format: ip/port">>}, Req, State}
	end;							
websocket_handle({text, <<Msg/binary>>}, Req, State) ->
	{reply, {text, <<"unsupport msg:", Msg/binary>>}, Req, State};
websocket_handle(_Data, Req, State) ->
	{ok, Req, State}.

websocket_info({timeout, _Ref, Msg}, Req, State) ->
	{reply, {text, Msg}, Req, State};
websocket_info({frame, Type, Time, Frame}, Req, State) ->
	Data = extend104_util:bin_to_str(extend104_frame:serialise(Frame), 16),
	Resp = lists:concat([Type, ":", Data, "(", extbif:strftime(Time), ")"]),
	{reply, {text, Resp}, Req, State};	
websocket_info(_Info, Req, State) ->
	?INFO("get info:~p, ~p", [self(), Req]),
	{ok, Req, State}.

websocket_terminate(_Reason, _Req, #state{conn_pid=CP}) ->
	?INFO("game over :~p",[self()]),
	[extend104_connection:unsubscribe(ConnPid, self())||ConnPid <- CP],
	ok.

parse_conn(ConnConf) ->
	[Ip, Port|_] = binary:split(ConnConf, <<"/">>),
	{ok, #extend104_oid{ip=Ip, port=binary_to_integer(Port)} }.
	
%%%----------------------------------------------------------------------
%%% Author  : hejin1026@gmail.com
%%% Purpose : http send packet
%%% Created : 2013 10 28
%%%----------------------------------------------------------------------
-module(master_websocket).

-author('hejin1026@gmail.com').

-include_lib("elog/include/elog.hrl").

-behaviour(cowboy_websocket_handler).

-export([init/3,
		websocket_init/3,
		websocket_handle/3,
		websocket_info/3,
		websocket_terminate/3]).

-record(state, {cid}).

%Websocket long connect cid:self()=1:n

init({tcp, http}, _Req, _Opts) ->
	?INFO("init get ws:~p", [self()]),
	{upgrade, protocol, cowboy_websocket}.

websocket_init(_TransportName, Req, _Opts) ->
	erlang:start_timer(2000, self(), <<"Hello!Send Req First...">>),
	{ok, Req, #state{}}.

websocket_handle({text, <<"connection:", Cid/binary>>}, Req, State) ->
	try 
		Result = master_dist:subscribe(binary_to_integer(Cid), self()),
		?INFO("subscribe:~p", [Result]),
		case Result of
			ok ->
				{reply, {text, << "begin to recv frame form ", Cid/binary >>}, Req, State#state{cid=binary_to_integer(Cid)}};
			{error, _Reason} ->
				{reply, {text, << "error for recv frame ", Cid/binary >>}, Req, State}
		end					
	catch _:Error ->
		?ERROR("get conn error:~p", [Error]),
		{reply, {text, << "connection info format cid">>}, Req, State}
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

websocket_terminate(_Reason, _Req, #state{cid=Cid}=State) ->
	?INFO("game over :~p",[self()]),
	master_dist:unsubscribe(Cid, self()),
	ok.
	
%%%----------------------------------------------------------------------
%%% Author  : hejin1026@gmail.com
%%% Purpose : http send packet
%%% Created : 2014 7 11
%%%----------------------------------------------------------------------
-module(pavilion_websocket).

-author('hejin1026@gmail.com').

-include_lib("elog/include/elog.hrl").

-behaviour(cowboy_websocket_handler).

-export([init/3,
		websocket_init/3,
		websocket_handle/3,
		websocket_info/3,
		websocket_terminate/3]).

-record(state, {cid, status, cache, count}).

-define(MAX_CACHE, 1024*1024*100).

-define(WEB_FILEPATH, "~/WebServer/sensorui/public/monitor/files").

-import(extbif, [to_binary/1]).

%Websocket long connect cid:self()=1:n

init({tcp, http}, _Req, _Opts) ->
	?INFO("init get ws:~p", [self()]),
	{upgrade, protocol, cowboy_websocket}.

websocket_init(_TransportName, Req, _Opts) ->
	erlang:start_timer(2000, self(), <<"Hello!Send Req First...">>),
	{ok, Req, #state{}}.

websocket_handle({text, <<"connection:", Cid/binary>>}, Req, State) ->
	try 
		Result = pavilion:subscribe(binary_to_integer(Cid), self()),
		?INFO("subscribe:~p", [Result]),
		case Result of
			true ->
				{reply, {text, << "begin to recv frame form ", Cid/binary >>}, Req, 
							State#state{status=connect,cid=binary_to_integer(Cid)}};
			{error, Reason} ->
				?ERROR("subscribe fail:~p, ~p", [Cid, Reason]),
				{reply, {text, << "error for recv frame ", Cid/binary >>}, Req, State}
		end
	catch _:Error ->
		?ERROR("get conn error:~p", [Error]),
		{reply, {text, << "connection error">>}, Req, State}
	end;
	
websocket_handle({text, <<"cache:", CidName/binary>>}, Req, #state{status=Status, cache=Queue} = State) ->
	case Status of
		undefined ->
			{reply, {text, <<"need to subscribe first!">>}, Req, State};	
		connect ->	
			{reply, {text, <<"begin to cache!">>}, Req, State#state{status=cache, cache=queue:new(), count=0}};	
		cache ->
			Data = string:join(queue:to_list(Queue), "\r\n"),
			?INFO("cache data:~p", [Data]),
			save_file(["data"], binary_to_list(CidName), Data),
			{reply, {text, <<"cache over!">>}, Req, State#state{status=connect, cache=queue:new(), count=0}}
	end;		
						
		
websocket_handle({text, <<Msg/binary>>}, Req, State) ->
	{reply, {text, <<"unsupport msg:", Msg/binary>>}, Req, State};
websocket_handle(_Data, Req, State) ->
	{ok, Req, State}.

websocket_info({timeout, _Ref, Msg}, Req, State) ->
	{reply, {text, Msg}, Req, State};
websocket_info({status, Cid, Time, Connect}, Req, State) ->
	Resp = [{time, to_binary(extbif:strftime(Time))}, {type, status}, {data, Connect}, {cid, Cid}],
	{reply, {text,  mochijson2:encode(Resp)}, Req, State};	
websocket_info({frame, Type, Time, Frame}, Req, State) ->
	Data = extend104_util:bin_to_str(extend104_frame:serialise(Frame), 16),
	Resp = [{time, to_binary(extbif:strftime(Time))}, {type, Type}, {data, to_binary(Data)}],
	WriteResp = lists:concat([Type, ":", Data, "(", extbif:strftime(Time), ")"]),
	{reply, {text, mochijson2:encode(Resp)}, Req, handle_cache(WriteResp, State)};	
websocket_info(_Info, Req, State) ->
	?INFO("get info:~p, ~p", [self(), Req]),
	{ok, Req, State}.

websocket_terminate(_Reason, _Req, #state{cid=Cid}) ->
	?INFO("game over :~p",[self()]),
	pavilion:unsubscribe(Cid, self()),
	ok.
	

handle_cache(Resp, #state{status=cache, cache=Data, count=Count} = State) ->
	if(Count+length(Resp) =< ?MAX_CACHE) ->
		State#state{cache=queue:in(Resp, Data), count=Count+length(Resp)};
	true ->
		{{value, First}, Rest} = queue:out(Data),
		State#state{cache=queue:in(Resp, Rest), count=Count+length(Resp)-length(First)}
	end;		
handle_cache(_Resp, State) ->	
	State.
	
save_file(Folder, Name, Data) ->
	?INFO("save folder:~p", [Folder]),
    Dir = filename:join(Folder),
    case filelib:is_dir(Dir) of
         true ->
            ?INFO("folder exist ...",[]),
            ok;
         false ->
            ?INFO("create folder ...",[]),
            filelib:ensure_dir(Dir),
            file:make_dir(Dir)
     end,
	 % {H,MM,S} =  time(),
     FileName = lists:concat([Name, ".txt"]),
     ?INFO("create file :~p",[FileName]),
     File = filename:join([Dir, FileName]),
     case file:open(File, [write, raw]) of
        {ok, Fd} ->
            ?INFO("write file: ~p",[File]),
            file:write(Fd, Data),
            file:close(Fd),
			os:cmd(lists:concat(["scp ", File, " ", ?WEB_FILEPATH]));
			
        Error ->
            ?WARNING("file open error :~p", [Error]),
            Error
     end.	

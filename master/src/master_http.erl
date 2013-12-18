
-module(master_http).

-author('hejin1026@gmail.com/2013-11-4').

-include("extend104.hrl").

-include_lib("elog/include/elog.hrl").

-export([init/3,
		handle/2,
		terminate/3]).
		
-define(MAX_SIZE, 1024*1024).		

-import(extend104_util, [b2a/1]).


init(_Transport, Req, []) ->
	{ok, Req, undefined}.

handle(Req, State) ->
    case cowboy_req:body_length(Req) of
    {Length, Req1} when is_integer(Length) and (Length > ?MAX_SIZE) -> 
		{ok, reply400(Req1, "body is too big."), State}; 
	{_, Req1} ->
		{Method, Req2} = cowboy_req:method(Req1),
		{Path, Req3} = cowboy_req:path(Req2),
		QsFun = qsfun(Method),
		{ok, Vals, Req4} = QsFun(Req3),
		Params = [{b2a(K), V} || {K, V} <- Vals],
		handle(Req4, Method, Path, Params, State)
	end.
	
qsfun(<<"GET">>) -> 
	fun(Req) -> {Vals, Req1} = cowboy_req:qs_vals(Req), {ok, Vals, Req1} end;

qsfun(<<"POST">>) ->
	fun(Req) -> cowboy_req:body_qs(?MAX_SIZE, Req) end.	

handle(Req, <<"GET">>, <<"/measure">>, Params, State) ->
    Cid = proplists:get_value(id, Params),
	MeaType = proplists:get_value(measure_type, Params, '$_'),
	MeaNo = proplists:get_value(measure_no, Params, '$_'),
	{ok, Meas} = master_datalog:get_measure({Cid, MeaType, MeaNo}),
	?INFO("get meas:~p", [Meas]),
	{ok, Reply} = cowboy_req:reply(200, [{"Content-Type", "text/plain"}], format(Meas), Req), 
	{ok, Reply, State};
	
handle(Req, Method, Path, Params, State) ->
	{Peer, Req1} = cowboy_req:peer(Req),
    ?ERROR("bad request from ~p: ~p ~p: ~p", [Peer, Method, Path, Params]),
	{ok, Reply} = cowboy_req:reply(400, [{"Content-Type", "text/plain"}], <<"bad request">>, Req1), 
	{ok, Reply, State}.

terminate(_Reason, _Req, _State) ->
	ok.	
	
reply400(Req, Msg) ->
	{ok, Reply} = cowboy_req:reply(400, [], Msg, Req), 
	Reply.	
	
format(Meas) ->
	string:join(lists:map(fun(#measure{id=Id, value=V}) ->
		lists:concat([Id#measure_id.cid, '-', Id#measure_id.type,'-', Id#measure_id.no,':'])  ++ to_string(V)
	end, Meas),"\n").

to_string(T)  ->
    lists:flatten(io_lib:format("~p", [T])).	
	
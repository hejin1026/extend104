
-module(inter_http).

-include_lib("elog/include/elog.hrl").

-export([init/3,
		handle/2,
		terminate/3]).
		
-define(MAX_SIZE, 1024*1024).		

init(_Transport, Req, []) ->
	{ok, Req, undefined}.

handle(Req, State) ->
	?INFO("get req...", []),
    case cowboy_req:body_length(Req) of
    {Length, Req1} when is_integer(Length) and (Length > ?MAX_SIZE) -> 
		{ok, reply400(Req1, "body is too big."), State}; 
	{_, Req1} ->
		{Method, Req2} = cowboy_req:method(Req1),
		{Path, Req3} = cowboy_req:path(Req2),
		QsFun = qsfun(Method),
		{ok, Vals, Req4} = QsFun(Req3),
		Params = [{extbif:to_atom(K), V} || {K, V} <- Vals],
		handle(Req4, Method, Path, Params, State)
	end.
	
qsfun(<<"GET">>) -> 
	fun(Req) -> {Vals, Req1} = cowboy_req:qs_vals(Req), {ok, Vals, Req1} end;

qsfun(<<"POST">>) ->
	fun(Req) -> cowboy_req:body_qs(?MAX_SIZE, Req) end.	

handle(Req, <<"GET">>, <<"/test_value.json">>, Params, State) ->
    Cid = case proplists:get_value(cid, Params, <<"">>) of
		<<"">> -> <<"0">>;
	    C -> C
		end,			
	?INFO("get cid:~p", [Cid]),
	Data = case inter:lookup_data(binary_to_integer(Cid)) of
		[] ->
			[];
		[{_, Datalist}] ->
			lists:map(fun([_, Key, Value, Time]) ->
				[{key, list_to_binary(Key)}, {value, Value}, {time, list_to_binary(Time)}]
			end, lists:reverse(Datalist))
		end,			
	{ok, Reply} = cowboy_req:reply(200, [{"Content-Type", "text/plain"}], jsonify(Data), Req), 
	{ok, Reply, State};
	
handle(Req, <<"GET">>, <<"/recv_value">>, Params, State) ->
    Key = proplists:get_value(key, Params, <<"">>), 
	?INFO("get recv value:~p", [Key]),
	Data = case inter:lookup_value(binary_to_list(Key)) of
		[] ->
			[];
		[{_, Value, Time}] ->
			[{key, Key}, {value, list_to_binary(Value)}, {time, list_to_binary(extbif:strftime(extbif:datetime(Time))) }]
		end,
	?INFO("made...",[]),				
	{ok, Reply} = cowboy_req:reply(200, [{"Content-Type", "text/plain"}], jsonify(Data), Req), 
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
	
jsonify(Term) ->
    Encoder = mochijson2:encoder([{utf8,true}]),
    Encoder(Term).	
	
	
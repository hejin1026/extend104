-module(inter_ctl).

-compile(export_all).

-include_lib("elog/include/elog.hrl").

run() ->
	{ok, Conf} = application:get_env(inter, mqtt), 
	Host = proplists:get_value(host, Conf),
	Port = proplists:get_value(port, Conf),
	{ok, PortNum} = application:get_env(inter, port_num),
	{ok, ConnNum} = application:get_env(inter, conn_num),  
	{ok, Offset} = application:get_env(inter, cid_offset),  
	?ERROR("start run portn:~p", [PortNum]),
	spawn(fun() -> 
		try
			lists:foreach(fun(N) ->
				PortN = Port+N,
				inter:go([{ip, Host}, {port, PortN}, {id, lists:concat([Host, "/", PortN])}]),
				?ERROR("send, port:~p, conn:~p", [PortN, ConnNum]),
				send_data(Host, PortN, Offset, ConnNum)
			end,lists:seq(0, PortNum-1))
		catch
			_:Err -> ?ERROR("send error: ~p, ~p", [Err, erlang:get_stacktrace()])
		end,	
	    ?ERROR("finish run conn:~p", [ConnNum])
	end).	
        
send_data(Host, Port, Offset, ConnNum) ->	
lists:foreach(fun(N) ->
		Cid = Offset + (Port - 1883) * ConnNum + N,
		?INFO("send, port:~p, conn:~p", [Port, Cid]),
		inter:send_data([{id, Cid},{ip, Host}, {port, Port}])
	end,lists:seq(1, ConnNum)).

lookup_inter(Cid) ->
	ets:lookup(inter_interval, list_to_integer(Cid)).	
	
lookup_data(Cid) ->
	inter:lookup_data(list_to_integer(Cid)).	
	
lookup_value(Key) ->
	inter:lookup_value(Key).		
	
lookup_emqtt(Ip, Port) ->
	Pid = inter:lookup_emqtt(Ip, list_to_integer(Port)),
	process_info(Pid, [registered_name, memory, message_queue_len,heap_size,total_heap_size]).

process(Process) ->
    io:format("porcess info:~p",[process_info(whereis(list_to_atom(Process)),
        [memory, message_queue_len,heap_size,total_heap_size])]).
			
			
			
run2() ->
	{ok, Conf} = application:get_env(inter, mqtt), 
	?INFO("get conf:~p", [Conf]),
	Host = proplists:get_value(host, Conf),
	Sql = "select CONCAT('test',t3.id) as id,t1.ip, t1.port from channels t1, protocols t2, term_station t3
           	where t1.channel_type =0 and t1.protocol_id=t2.id and t1.station_id =t3.id and t2.code = 'mqtt' and t1.ip='" ++ Host ++ "' 
			group by t1.port",
	case emysql:sqlquery(Sql) of
        {ok, Records} ->
			?ERROR("start run ~p: num:~p ~n", [?MODULE, length(Records)]),
			try
				lists:foreach(fun(Record) ->
					inter:go(Record)
				end, Records),
				send_data2()
			catch
				_:Err -> ?ERROR("dispatch error: ~p, ~p", [Err, erlang:get_stacktrace()])
			end,
            ?ERROR("finish run ~p: ~p ~n", [?MODULE, length(Records)]);
        {error, Reason}  ->
            ?ERROR("start failure...~p",[Reason]),
            []
	end.
	
send_data2() ->	
	Sql = "select t1.id, t1.ip, t1.port from channels t1, protocols t2
			where t1.channel_type =0 and t1.protocol_id=t2.id and t2.code = 'mqtt'",
	case emysql:sqlquery(Sql) of
        {ok, Records} ->
			?ERROR("start run channel ~p: num:~p ~n", [?MODULE, length(Records)]),
			lists:foreach(fun(Record) ->
					inter:send_data(Record)
				end, Records),
			?ERROR("finish run ~p: ~p ~n", [?MODULE, length(Records)]);
        {error, Reason}  ->
            ?ERROR("start failure...~p",[Reason]),
            []
	end.
	
	
lookup(Ip, Port) ->
	ets:lookup(port_cid, {list_to_binary(Ip), list_to_integer(Port)}).	

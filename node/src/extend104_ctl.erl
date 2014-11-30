%%%----------------------------------------------------------------------
%%% Created	: 2013-12-4
%%% author 	: hejin1026@gmail.com
%%%----------------------------------------------------------------------
-module(extend104_ctl).

-include("extend104.hrl").
-include_lib("elog/include/elog.hrl").

-compile(export_all).

status() ->
    {InternalStatus, _ProvidedStatus} = init:get_status(),
    ?PRINT("Node ~p is ~p.", [node(), InternalStatus]),
    case lists:keysearch(extend104, 1, application:which_applications()) of
	false ->
		"node is not running~n";
	{value,_Version} ->
		"node is running~n"
    end.
	
process(Process) ->
    process_info(whereis(list_to_atom(Process)),
        [memory, message_queue_len,heap_size,total_heap_size]).	
		%
% process2(P, I, D) ->
%     process_info(pid(list_to_integer(P),list_to_integer(I),list_to_integer(D)),
%         [memory, message_queue_len,heap_size,total_heap_size]).
	
state(Type) ->
    sys:get_status(list_to_atom(Type)).

conn_status(Cid) ->
	extend104:conn_status(list_to_integer(Cid)).
	
lookup(Key) ->
	extend104_hub:lookup(list_to_binary(Key)).	
	
lookup_ertdb() ->
	process_info(extend104_hub:lookup_ertdb(), [memory, message_queue_len,heap_size,total_heap_size]).	
	
client_info(Cid) ->
	case extend104:get_conn_pid(list_to_integer(Cid)) of
		{ok, ConnPid} ->
			process_info(ConnPid,[memory, message_queue_len,heap_size,total_heap_size]);
		error ->
			no_conn
	end.
	

% test
% ./bin/node -sname node1 send_data 2 15 2 151834
send_data(Tid, Type, No, Value) ->
	DateTime = extbif:timestamp(),
	Data = #measure{type=list_to_integer(Type), no = list_to_integer(No), value = list_to_integer(Value)},
	extend104_hub:send_datalog({measure, list_to_integer(Tid), DateTime, [Data]}). 
	
publish_data(Ip, Port, Cid) ->
    case emqtt_client:start_link([{ip, Ip},{port, list_to_integer(Port)}, {id, Cid}]) of
        {ok, ConnPid} ->
			?INFO("add mqtt cid:~p", [Cid]),
			emqtt_client:publish(ConnPid, {"measure", 1, {measure, [{"1:11:23", 345}, {"1:1:34", 123}]} });
			
        {error, Error} ->
            ?ERROR("get conn error: ~p", [Error]),
			{error, Error}
     end.
	
	
%% set %%
stop() ->
	extend104_app:stop(),
    init:stop().

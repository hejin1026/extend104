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
	
state(Type) ->
    sys:get_status(list_to_atom(Type)).

conn_status(Cid) ->
	extend104:conn_status(list_to_integer(Cid)).
	
lookup(Key) ->
	extend104_hub:lookup(list_to_binary(Key)).	
	

% test
% ./bin/node -sname node1 send_data 2 15 2 151834
send_data(Tid, Type, No, Value) ->
	DateTime = extbif:timestamp(),
	Data = #measure{type=list_to_integer(Type), no = list_to_integer(No), value = list_to_integer(Value)},
	extend104_hub:send_datalog({measure, 2, DateTime, [Data]}). 
	
	
%% set %%
stop() ->
	extend104_app:stop(),
    init:stop().

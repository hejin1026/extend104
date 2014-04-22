%%%----------------------------------------------------------------------
%%% Created	: 2013-12-4
%%% author 	: hejin1026@gmail.com
%%%----------------------------------------------------------------------
-module(master_ctl).

-include_lib("elog/include/elog.hrl").
-include("terminl.hrl").

-compile(export_all).

run() ->
	Dispatch = fun(Cid) ->
		case term:lookup(Cid) of
		{ok, #channel{cid=Cid, data=Data}} ->
			master_dist:dispatch({monitor, Cid, Data});
		{false, _} -> 
			?ERROR("~p is not found", [Cid])
		end
	end,
	spawn(fun() -> 
		AllCid = term:all_channel(),
		?INFO("begin to dispatch ~p entries...", [length(AllCid)]),
		try
			lists:foreach(Dispatch, AllCid)
		catch
			_:Err -> ?ERROR("dispatch error: ~p", [Err])
		end
	end).
	
config() ->	
	Config = fun(Record) ->
			?INFO("record :~p", [Record]),
			Cid = proplists:get_value(cid, Record),
			No = proplists:get_value(no, Record),
			Category = proplists:get_value(category, Record),
			Key = proplists:get_value(key, Record),
			Value = build_config(Record, []),
			Cmd = ["config", Key, Value],
			master:config(Cmd)
		end,
	spawn(fun() ->
			case master:ertdb(connect) of
				ok ->
					Sql = "select t3.id as cid, t1.* 
							from term_measure t1, term_station t2, channel t3 
							where t1.station_id=t2.id and t2.id=t3.station_id",
					case emysql:sqlquery(Sql) of
				        {ok, Records} ->
				            lists:foreach(Config, Records),
				            ?ERROR("finish from station ~p: ~p ~n", [?MODULE, length(Records)]);
				        {error, Reason}  ->
				            ?ERROR("start failure...~p",[Reason]),
				            stop
					end,
					master:ertdb(close);
				{error, Reason} ->
					?ERROR("ertdb connect error:~p", [Reason])	
			end
		end).	
	
sync() ->
	Dispatch = fun(Cid) ->
		master_dist:dispatch({sync, Cid})
	end,
	spawn(fun() -> 
		AllCid = term:all_channel(),
		?INFO("begin to dispatch ~p entries...", [length(AllCid)]),
		try
			lists:foreach(Dispatch, AllCid)
		catch
			_:Err -> ?ERROR("dispatch error: ~p", [Err])
		end
	end).	
	
		

status() ->
    {InternalStatus, _ProvidedStatus} = init:get_status(),
    ?PRINT("Node ~p is ~p.", [node(), InternalStatus]),
    case lists:keysearch(master, 1, application:which_applications()) of
	false ->
		?PRINT_MSG("master is not running~n");
	{value,_Version} ->
		?PRINT_MSG("master is running~n")
    end.

build_key(Cid, Type, No) ->
	list_to_binary(lists:concat([Cid, ":", Type, ":", No])).	
	
build_config([], Acc) ->
	 string:join([lists:concat([K, "=", strnum(V)]) || {K, V} <- Acc], ",");	
build_config([{coef, Value}|Data], Acc) ->	
	build_config(Data, [{coef, Value}|Acc]);
build_config([{offset, Value}|Data], Acc) ->	
	build_config(Data, [{offset, Value}|Acc]);		 
build_config([{deviation, Value}|Data], Acc) ->	
	build_config(Data, [{dev, Value}|Acc]);
build_config([{maxtime, Value}|Data], Acc) ->
	build_config(Data, [{maxtime, Value}|Acc]);	
build_config([{mintime, Value}|Data], Acc) ->
	build_config(Data, [{mintime, Value}|Acc]);		
build_config([{his_compress, Value}|Data], Acc) ->
	build_config(Data, [{compress, Value}|Acc]);	
build_config([{his_deviation, Value}|Data], Acc) ->
	build_config(Data, [{his_dev, Value}|Acc]);			
build_config([{his_maxtime, Value}|Data], Acc) ->
	build_config(Data, [{his_maxtime, Value}|Acc]);	
build_config([{his_mintime, Value}|Data], Acc) ->
	build_config(Data, [{his_mintime, Value}|Acc]);			
build_config([_|Data], Acc) ->
	build_config(Data, Acc).	
	
strnum(V) when is_integer(V) ->
    integer_to_list(V);
strnum(V) when is_float(V) ->
    [S] = io_lib:format("~.6f", [V]), S;
strnum(Other) ->
	Other.
		
	
	
	
	
	
	
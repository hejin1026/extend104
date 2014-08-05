%%%----------------------------------------------------------------------
%%% Author  : hejin1026@gmail.com
%%% Purpose : http send packet
%%% Created : 2013-12-9
%%%----------------------------------------------------------------------
-module(extend104_hub).

-include("extend104.hrl").
-include_lib("elog/include/elog.hrl").

-export([start_link/1,
		config/2,lookup/1,
        send_datalog/1,
		stop/0]).

-behavior(gen_server).

-export([init/1,
        handle_call/3,
        handle_cast/2,
        handle_info/2,
        terminate/2,
        code_change/3]).

-record(state, {channel, ertdb}).

-record(last, {key, type, ptype, coef, time, value}).

-import(extbif, [to_atom/1]).

%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link(Config) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Config], []).
	
config(Key, Data) ->
	gen_server:call(?MODULE, {config, Key, Data}).	

send_datalog(DataList) ->
    gen_server:cast(?MODULE, DataList).
	
lookup(Key) ->
	ets:lookup(last, Key).	


stop() ->
    gen_server:call(?MODULE, stop).

init([Config]) ->
    ?INFO("Monet hub is starting...[done]", []),
	ets:new(last, [set, protected, named_table, {keypos, #last.key}]),
    {ok, Conn} = amqp:connect(),
    Channel = open(Conn),
	{ok, Client} = ertdb_client:start_link(Config),
    {ok, #state{channel = Channel, ertdb=Client}}.

open(Conn) ->
    {ok, Channel} = amqp:open_channel(Conn),
    Channel.

handle_call(stop, _From, State) ->
    {stop, normal, ok, State};
	
handle_call({config, Key, Data}, From, State) ->
	?INFO("insert config:~p, ~p", [Key,Data]),
	Rest = case proplists:get_value(type, Data) of
		'undefined' ->
			ets:delete(last, Key);
		Type ->	
			Ptype = proplists:get_value(ptype, Data),
			Coef = proplists:get_value(coef, Data),
			case ets:lookup(last, Key) of
				[] ->
					ets:insert(last, #last{key=Key, type=Type, ptype=Ptype, coef=Coef});
				[Config] ->
					ets:insert(last, Config#last{key=Key, type=Type, ptype=Ptype, coef=Coef})
			end
	end,					
	{reply, Rest, State};	

handle_call(Req, _From, State) ->
	?ERROR("badreq: ~p", [Req]),
    {reply, {badreq, Req}, State}.

handle_cast({measure, Tid, DateTime, DataList}, #state{channel = Channel, ertdb=Client} = State) ->
    ?INFO("send datalog: ~p, ~p, ~p",[Tid,DateTime, DataList]),
	lists:foreach(fun(Meas) ->
		Key = build_key(Tid, Meas#measure.type, Meas#measure.no),
		Cmd = ["insert", Key, DateTime, extbif:to_list(Meas#measure.value)],
		ok = ertdb_client:q_noreply(Client, Cmd),
		
		%% for stat
	    case ets:lookup(last, Key) of
		    [#last{type=Type, ptype=Ptype, coef=Coef, time=LastTime, value=LastValue} = Config] -> 
				if(LastTime == undefined) ->
					?INFO("insert first:~p,~p", [Meas#measure.value, Config]),
					ets:insert(last, Config#last{time=DateTime, value=Meas#measure.value});	
				true ->
					Interval = DateTime - LastTime,
					if Interval > 0 ->
						try format_value(Type, Coef, Interval, LastValue, Meas#measure.value) of
							{insert, Value} ->
								?INFO("insert value:~p, ~p", [Meas#measure.value, Config]),
								ets:insert(last, Config#last{time=DateTime, value=Meas#measure.value}),
								%% 改统计方式，脚本统计
								% Datalog = [{ekey, Key},{station_id, Tid},{ptype, Ptype},
									% {time, extbif:datetime(DateTime)},{value, Value}],
								% amqp:send(Channel, <<"measure.datalog">>, term_to_binary({datalog, Key, Datalog}));
								StatKey = list_to_binary([Key, ":stat"]),
								StatCmd = ["insert", StatKey, DateTime, extbif:to_list(Value)],
								ertdb_client:q_noreply(Client, StatCmd);
								
							insert ->
								?INFO("insert datalog:~p, ~p", [Meas#measure.value, Config]),
								ets:insert(last, Config#last{time=DateTime, value=Meas#measure.value});	
							ignore ->
								?WARNING("ignore value:~p, ~p", [Meas#measure.value, Config])
			            catch
			                _:Err ->
			                    ?ERROR("err: ~p, type: ~p, key: ~p, ~p", [Err, Type, Key, erlang:get_stacktrace()]),
			                    []
			            end;			
					true ->
						?WARNING("~p: interval < 0", [Key])
					end
				end;					
		    [] -> ok
	    end
		
	end, DataList),
    {noreply, State};

handle_cast(Msg, State) ->
    ?ERROR("Unexpected message: ~p", [Msg]),
    {noreply, State}.

handle_info(Info, State) ->
    ?WARNING("unext info :~p", [Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------
build_key(Cid, Type, No) ->
	list_to_binary(lists:concat([Cid, ":", Type, ":", No])).


%% status : 1 -> 0 记录 | 0 -> 1 忽略
format_value(<<"status">>, Coef, Interval, LastValue, Value) ->
	case Value of
		LastValue ->
			%TODO should ignore
			{insert, 0};
		_ ->
			case Value of
				0 -> 
					if(Interval > 60 * 10) ->
						{insert, 1};
					true ->
						insert
					end;
				1 ->
					insert;
				_ ->
					?ERROR("invaild status value:~p", [Value, LastValue]),
					ignore	
			end
	end;	
format_value(<<"dev">>, Coef, Interval, LastValue, Value) ->	
	Dev = extbif:to_integer(Value) - extbif:to_integer(LastValue),
	if(Dev >= 0) -> %TODO should > 
		{insert, Dev * Coef};
	true ->
		ignore
	end;		
format_value(Type, Coef, Interval, LastValue, Value) ->
	?INFO("unsupport type:~p", [Type]),
	ignore.
	

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

-record(last, {key, type, ptype, time, value}).

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
	?INFO("insert config:~p", [Key]),
	Ptype = proplists:get_value(ptype, Data),
	Type = format_ptype(Ptype),
	Rest = ets:insert(last, #last{key=Key, type=Type, ptype=Ptype}),
	{reply, Rest, State};	

handle_call(Req, _From, State) ->
	?ERROR("badreq: ~p", [Req]),
    {reply, {badreq, Req}, State}.

handle_cast({measure, Tid, DateTime, DataList}, #state{channel = Channel, ertdb=Client} = State) ->
    ?INFO("send datalog: ~p, ~p",[Tid,DateTime, DataList]),
	lists:foreach(fun(Meas) ->
		Key = build_key(Tid, Meas#measure.type, Meas#measure.no),
		Cmd = ["insert", Key, DateTime, Meas#measure.value],
		ok = ertdb_client:q_noreply(Client, Cmd),
		
		%% for stat
	    case ets:lookup(last, Key) of
		    [#last{type=Type, ptype=Ptype, time=LastTime, value=LastValue} = Config] -> 
			Interval = DateTime - LastTime,
				if Interval > 0 ->
					try format_value(Type, Interval, LastValue, Meas#measure.value) of
						{insert, Value} ->
							ets:insert(last, Config#last{time=DateTime, value=Meas#measure.value}),
							Datalog = [{key, Key}, {station_id, Tid}, {ptype, Ptype}, {time, DateTime}, {value, Value}],
							amqp:send(Channel, <<"measure.datalog">>, term_to_binary({datalog, Key, Datalog}));
						insert ->
							ets:insert(last, #last{key=Key, type=Type, time=DateTime, value=Meas#measure.value});	
						ignore ->
							?WARNING("ignore value:~p, ~p", [Meas#measure.value, Config])
		            catch
		                _:Err ->
		                    ?ERROR("err: ~p, type: ~p, key: ~p, ~p", [Err, Type, Key, erlang:get_stacktrace()]),
		                    []
		            end;			
				true ->
					?WARNING("~p: interval < 0", [Key])
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


format_ptype(280) -> status;	
format_ptype(999) -> status;	
format_ptype(230) -> dev;	
format_ptype(100) -> dev;	
format_ptype(_Value) -> none.

%% status : 1 -> 0 记录 | 0 -> 1 忽略
format_value(status, Interval, LastValue, Value) ->
	case Value of
		LastValue ->
			case Value of
				"0" -> 
					if(Interval > 60 * 10) ->
						{insert, 1};
					true ->
						insert
					end;
				"1" ->
					insert
			end;	
		_ ->
			ignore
	end;	
format_value(dev, Interval, LastValue, Value) ->	
	Dev = extbif:to_integer(Value) - extbif:to_integer(LastValue),
	if(Dev > 0) ->
		{insert, Dev};
	true ->
		ignore
	end;		
format_value(_Type, Interval, LastValue, Value) ->
	ignore.
	

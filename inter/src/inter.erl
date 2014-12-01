
-module(inter).

-include_lib("elog/include/elog.hrl").

-behavior(gen_server).

-export([start_link/0, go/1, send_data/1, save_data/2, lookup_data/1, lookup_value/1]).

-export([init/1, 
		handle_call/3, 
		handle_cast/2, 
		handle_info/2, 
		terminate/2, 
		code_change/3]).

-record(state, {channel, num, interval}).	

		
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
	
go(Config) ->
	gen_server:call(?MODULE, {go, Config}).	
		
send_data(Channel) ->
	gen_server:call(?MODULE, {send_data, Channel}).		
	
save_data(Cid, Data) ->
	gen_server:cast(?MODULE, {save_data, Cid, Data}).	
	
lookup_data(Cid) ->
	ets:lookup(inter_channel, Cid).
	
lookup_value(Key) ->
	ets:lookup(inter_recv, Key).	
			
init([]) ->
	process_flag(trap_exit, true),
	ets:new(inter_channel, [set, named_table]),
	ets:new(inter_recv, [set, named_table]),
	{ok, Num} = application:get_env(key_num),
	{ok, Interval} = application:get_env(interval),
    {ok, #state{channel = dict:new(), num=Num, interval=Interval}}.
	
	
handle_call({go, Config}, _From, State=#state{channel=CS}) ->
	?INFO("go:~p",[Config]),
	Ip = proplists:get_value(ip, Config),
	Port = proplists:get_value(port, Config),
	{ok,C}=emqtt_client:start_link(Config),
	Topic = lists:concat(["command/",Ip, "/", Port]),
	emqtt_client:subscribe(C, {Topic, 1}),
	emqtt_client:consume(C, inter),
	{reply, ok, State#state{channel= dict:store({Ip, Port}, C, CS)} };
	
handle_call({send_data, Channel}, _From, State=#state{interval=Interval}) ->
	erlang:send_after((Interval + random:uniform(Interval)) * 1000, self(), {send_data, Channel}),
	{reply, ok, State};	
	
handle_call(Req, _From, State) ->
    ?ERROR("Unexpected request: ~p", [Req]),
    {reply, ok, State}.


handle_cast({save_data, Cid, Data}, State) ->
	ets:insert(inter_channel, {Cid, Data}),
	{noreply, State};

handle_cast(Msg, State) ->
    ?ERROR("Unexpected message: ~p", [Msg]),
    {noreply, State}.


handle_info({send_data, Channel}, State=#state{channel=CS, num=Num, interval=Interval} ) ->
	?INFO("send_data:~p", [Channel]),
	Cid = proplists:get_value(id, Channel),
	Ip = proplists:get_value(ip, Channel),
	Port = proplists:get_value(port, Channel),
	EmqttC = dict:fetch({Ip, Port}, CS),
	spawn(fun() ->
        try send_data(EmqttC, Cid, Num) 
        catch
            _:Error ->
                ?ERROR("run_catch : ~p ~n Task: ~p ~n ~p", [Error, Channel, erlang:get_stacktrace()])
        end
	end),
	erlang:send_after(Interval * 1000, self(), {send_data, Channel}),
	{noreply, State};

handle_info({measure, Cid, DateTime, DataList}, State) ->
	?ERROR("get measure data:~p,~p", [Cid, DataList]),
	lists:foreach(fun({Key, Value}) ->
	    ets:insert(inter_recv, {Key, Value, DateTime})
	end, DataList),
    {noreply, State};

handle_info({'EXIT', Pid, Reason}, State) ->
	?ERROR("unormal exit message received: ~p, ~p", [Pid, Reason]),
	{noreply, State};	

handle_info(Info, State) ->
	?INFO("Unexpected info received: ~p", [Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
send_data(C, Cid, Num) ->
	Topic = lists:concat(["measure/",Cid]),
	
	% DateTime = {datetime, calendar:local_time()},
	StrfTime = extbif:strftime(),
	{Data, Data2} = lists:mapfoldl(fun(No,Acc) ->
			Key = lists:concat([Cid, ":11:", No]),
			random:seed(erlang:now()),
			Value = random:uniform(1000),
			{ {Key, Value}, [[Cid, Key, Value, StrfTime]|Acc] }
		end, [], lists:seq(0, Num-1)),
	
	Message =  {measure, Data},
	emqtt_client:publish(C, {Topic, 1, Message }),
	% delete_data(Cid),
% 	save_data(Data2),
	
	inter:save_data(Cid, Data2).

delete_data(Cid) ->
	?INFO("delete cid:~p", [Cid]),
    case emysql:delete(test_measure, {cid, Cid}) of
        {error, Reason} ->
            ?ERROR("delete cid:~p, ~n Reason: ~p", [Cid, Reason]);
        _ ->
            ok
    end.	

save_data(Data) ->
	?INFO("sava data:~p", [Data]),
    case emysql:insert(test_measure, [cid, ekey, value,realtime], Data) of
        {error, Reason} ->
            ?ERROR("insert host  :~p, ~n Reason: ~p", [Data, Reason]);
        _ ->
            ok
    end.
        
%%%----------------------------------------------------------------------
%%% Created	: 2013-12-4
%%% author 	: hejin1026@gmail.com
%%%----------------------------------------------------------------------
-module(master_dist).

-behavior(gen_server).

-include_lib("elog/include/elog.hrl").
-include_lib("amqp_client/include/amqp_client.hrl").

-export([start_link/0,
		dispatch/1,
		subscribe/2,
		unsubscribe/2
		]).

-export([init/1, 
		handle_call/3, 
		handle_cast/2, 
		handle_info/2, 
		terminate/2, 
		code_change/3]).

-record(dispatch, {id, node}).

-record(state, {channel}).

-import(extbif, [to_binary/1]).
		
dispatch(Payload) ->
	gen_server2:cast(?MODULE, {dispatch, Payload}).
	
subscribe(Cid, WebSocket) ->
	gen_server2:call(?MODULE, {subscribe, Cid, WebSocket}).	
	
unsubscribe(Cid, WebSocket) ->
	gen_server2:call(?MODULE, {unsubscribe, Cid, WebSocket}).		
	
start_link() ->
    gen_server2:start_link({local, ?MODULE}, ?MODULE, [], []).
		
init([]) ->
    {ok, Conn} = amqp:connect(),
    Channel = open(Conn),
    mnesia:create_table(dispatch, [{ram_copies, [node()]},
        {attributes, record_info(fields, dispatch)}]),
	ets:new(cid_wb, [bag, named_table]),	
    {ok, #state{channel = Channel}}.

open(Conn) ->
    {ok, Channel} = amqp:open_channel(Conn),
	amqp:queue(Channel, <<"monitor.inter">>),
	amqp:queue(Channel, <<"command.inter">>),
    amqp:queue(Channel, <<"monitor.reply">>),
    amqp:consume(Channel, <<"monitor.reply">>, self()),
	amqp:consume(Channel, <<"monitor.inter">>),
	amqp:consume(Channel, <<"command.inter">>),
    Channel.
	
handle_call({subscribe, Cid, WebSocket}, _From, #state{channel = Channel}=State) ->
	Reply = with_monitor(Cid, 
					fun(undefined) -> e({no_monitored, Cid});
						(Node) ->
							amqp:send(Channel, Node, term_to_binary({subscribe, Cid})),
							ets:insert(cid_wb, {Cid, WebSocket}),ok
						end),
	{reply, Reply, State};		

handle_call({unsubscribe, Cid, WebSocket}, _From, #state{channel = Channel}=State) ->
    Reply = case lookup(Cid) of
        [] ->
            {error, no_subscribe};
        [{Cid, WebSocket}] ->
			with_monitor(Cid, fun(undefined) -> e({no_monitored, Cid});
							(Node) ->
			            		amqp:send(Channel, Node, term_to_binary({unsubscribe, Cid})),
								ets:delete(cid_wb, Cid) 
							end);
		_WSPid ->
			ets:delete_object(cid_wb, {Cid, WebSocket})	
    end,
	{reply, Reply, State};	
		
	
handle_call(Msg, _From, State) ->
	?ERROR("unext call msg :~p", [Msg]),
	{reply, ok, State}.
	
	
handle_cast({dispatch, {monitor, Cid, _Data}=Payload}, State) ->
	handle_monitor(Payload, State),
	{noreply, State};	
	
handle_cast({dispatch, {config, Cid, Key, Data}=Payload},  #state{channel = Channel}=State) ->
	with_monitor(Cid, fun(undefined) -> e({no_monitored, Cid});
					(Node) -> amqp:send(Channel, Node, term_to_binary(Payload))
					end),
	{noreply, State};	

handle_cast({dispatch, {sync, Cid}}, #state{channel = Channel}=State) ->
	with_monitor(Cid, fun(undefined) -> e({no_monitored, Cid});
					(Node) -> amqp:send(Channel, Node, term_to_binary({sync, Cid}))
					end),
	{noreply, State};		
	
handle_cast({dispatch, {unmonitor, Cid}}, #state{channel = Channel}=State) ->
	with_monitor(Cid, fun(undefined) -> e({no_monitored, Cid});
					(Node) -> 
						amqp:send(Channel, Node, term_to_binary({unmonitor, Cid})),
						mnesia:dirty_delete(dispatch, {monitor, Cid})
					end),
	{noreply, State};
		
handle_cast(Msg, State) ->
	?ERROR("unext case msg :~p", [Msg]),
	{noreply, State}.
	
	
handle_info({deliver, <<"monitor.inter">>, _Properties, Payload}, State) ->
	%TODO from web message
	% Payload2 = parse_monitor(Payload),
	% handle_monitor(Payload2, State),
	{noreply, State};	
	
handle_info({deliver, <<"command.inter">>, Properties, Payload}, #state{channel = Channel}=State) ->
	?ERROR("recv command :~p, msg:~p", [Properties, Payload]),
	Data = mochijson2:decode(Payload, [{format, proplist}]),
	Cid = extbif:to_integer(proplists:get_value(<<"cid">>, Data)),
	Type = proplists:get_value(<<"type">>, Data),
	Params = proplists:get_value(<<"params">>, Data),
	CorrelationId = proplists:get_value(correlation_id, Properties),
	ReplyTo = proplists:get_value(reply_to, Properties),
	ReplyFun = fun(Result) ->
		    Props = #'P_basic'{correlation_id = CorrelationId},
			Response = mochijson2:encode([{result, Result}|Params]),
		    Publish = #'basic.publish'{exchange = <<>>,
		                               routing_key = ReplyTo,
		                               mandatory = true},
		    amqp_channel:call(Channel, Publish, #amqp_msg{props = Props,
		                                                  payload = list_to_binary(Response)})
		end,
	
	with_monitor(Cid, fun(undefined) -> ReplyFun(nomonitor_reply);
					(Node) -> 
						NewPayload = term_to_binary({command, Cid, {Type, Params}}),
					    Props = #'P_basic'{correlation_id = CorrelationId,
					                       content_type = <<"application/octet-stream">>,
					                       reply_to = ReplyTo},
					    Publish = #'basic.publish'{routing_key = extbif:to_binary(Node), mandatory = true},
					    amqp_channel:call(Channel, Publish, #amqp_msg{props = Props,payload = NewPayload})
					end, 
					fun() -> ReplyFun(nomonitor_cid) end),
	{noreply, State};		
	
handle_info({deliver, <<"monitor.reply">>, _Properties, Payload}, State) ->
    ?INFO("get monitor reply :~p", [binary_to_term(Payload)]),
    handle_reply(binary_to_term(Payload), State),
    {noreply, State};
	
handle_info(Msg, State) ->
	?ERROR("unexpected info: ~p", [Msg]),
	{noreply, State}.
	
	
terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
	
	
%%%----------------------------------------------------------------------
handle_monitor({monitor, Cid, Data}=Payload, #state{channel=Channel}) ->
    CityId = proplists:get_value(cityid, Data),
    Payload2 = term_to_binary(Payload),
	NodeId = master:get_queue(monitor, CityId),
	?INFO("get nodeid:~p", [NodeId]),
	with_monitor(Cid, fun(undefined) -> ?ERROR("has already monitor, no reply:~p", [Cid]);
						(Node) -> ?ERROR("has already monitor, send again:~p, in ~p", [Cid, Node]),
								amqp:send(Channel, Node, Payload2) end, 
			fun() -> 
				amqp:send(Channel, to_binary(NodeId), Payload2),
            	mnesia:dirty_write(#dispatch{id = {monitor, Cid}}) 
			end).	
		

handle_reply({monitored, Cid, MonitorQ, Node}, _State) ->
	with_monitor(Cid, fun(OldNode) ->
               if
                   OldNode == undefined -> ok;
                   OldNode == MonitorQ -> ok;
                   true -> ?ERROR("two nodes for one cid: ~p, oldnode: ~p, newnode: ~p", [Cid, OldNode, MonitorQ])
               end,
			   [NodeName, HostName] = string:tokens(atom_to_list(Node), "@"),
			   update_channel(Cid, [{node_name, NodeName}, {host_name, HostName}]),
               mnesia:dirty_write(#dispatch{id={monitor, Cid}, node=MonitorQ})
			end);
			
handle_reply({status, Cid, Connect}, _State) ->
	update_channel(Cid, [{connect_status, Connect}]);
    
handle_reply({frame, Cid, {Type, Time, Frame}}, #state{channel=Channel}) ->
	case lookup(Cid) of
		[] ->
			with_monitor(Cid, fun(Node)->
			            amqp:send(Channel, Node, term_to_binary({unsubscribe, Cid}))
					end);
		WSPid ->
			[Pid ! {frame, Type, Time, Frame} || {_, Pid} <- WSPid]
	end;			
handle_reply(_Reply, _State) ->
    ok.		

lookup(Cid) ->
	ets:lookup(cid_wb, Cid). 
	
update_channel(Cid, Info) ->
	DateTime = {datetime, {date(), time()}},
    case emysql:select({channels, {id, Cid}}) of
        {ok, [_Record|_]} ->
            case emysql:update(channels, [{updated_at, DateTime} | Info], {id, Cid}) of
                {error, Reason} ->
                    ?ERROR("update :~p, ~n Reason: ~p", [Info, Reason]);
                _ ->
                    ok
             end;
        {ok, []} ->
            ?ERROR("can not find channel:~p",[Cid]);
        {error, Reason} ->
            ?ERROR("~p",[Reason])
    end.
		

with_monitor(Cid, F) ->
	with_monitor(Cid, F, fun() -> e({no_dispatch, Cid}) end).
	
with_monitor(Cid, F, E) ->
	case mnesia:dirty_read(dispatch, {monitor, Cid}) of
        [] ->
			?ERROR("unmonitor: ~p", [{monitored, Cid}]),
            E();
        [#dispatch{node = Node}] ->
            F(Node)
    end.		

e(E) -> {error, E}.
		
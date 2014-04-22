-module(extend104).

-include("extend104.hrl").

-include_lib("elog/include/elog.hrl").

-export([start_link/2,
		open_conn/1,delete_conn/1,sync/1,
		get_conn_pid/1
		]).

-behavior(gen_server).

-export([init/1,
		handle_call/3,
		handle_cast/2,
        handle_info/2,
        code_change/3,
        terminate/2]).


-record(state, {cityid, channel, connection_sup, map_cid_pid = dict:new()}).

start_link(CityId, ConnectionSup) ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [CityId, ConnectionSup], []).
	
open_conn(ConnConf) ->
	gen_server:call(?MODULE, {open_conn, ConnConf}).	
	
delete_conn(Cid) ->
	gen_server:call(?MODULE, {delete_conn, Cid}).	
	
sync(Cid) ->
	gen_server:cast(?MODULE, {sync, Cid}).	
	
get_conn_pid(Oid) ->
	gen_server:call(?MODULE, {get_conn_pid, Oid}).		

init([CityId, ConnectionSup]) ->
    {ok, Conn} = amqp:connect(),
    Channel = open(Conn, CityId),
    % ?INFO("opengoss agent is starting...~p", [node()]),
	{ok, #state{cityid = CityId, channel = Channel, connection_sup = ConnectionSup}}.
	

open(Conn, CityId) ->
    {ok, Channel} = amqp:open_channel(Conn),
    QueueName = get_node_queue(),
    {ok, Q} = amqp:queue(Channel, QueueName),
    amqp:topic(Channel, <<"master.topic">>),
    amqp:bind(Channel, <<"master.topic">>, Q, CityId),
    amqp:consume(Channel, Q),
    amqp:send(Channel, <<"node.reply">>, term_to_binary({add, {node_id, CityId, node()}})),
    Channel.

get_node_queue() ->
    [NodeName|_] = string:tokens(atom_to_list(node()), "@"),
    NodeName ++ ".node".	
	
handle_connect(ConnConf, #state{connection_sup = ConnSup, map_cid_pid=MapCP} = State) ->
    case extend104_connection_sup:start_connection(ConnSup, ConnConf) of
        {ok, ConnPid} ->
			Cid = get_cid(ConnConf),
			{reply, {ok, ConnPid}, State#state{
					map_cid_pid=dict:store(Cid, ConnPid, MapCP)}};
        {error, Error} ->
            ?ERROR("get conn error: ~p, ~p", [Error, ConnConf]),
			{reply, {error, Error}, State}
     end.	
	
	
handle_call({open_conn, ConnConf}, _From, #state{map_cid_pid=MapCP} = State) ->
	Cid = get_cid(ConnConf),
	case dict:find(Cid, MapCP) of
		{ok, _ConnPid} ->
			{reply, {ok, already_conn}, State};
		error ->	
			handle_connect(ConnConf, State)
	end;
			
handle_call({delete_conn, Cid}, _From, #state{map_cid_pid = MapCP} = State) ->
	case dict:find(Cid, MapCP) of
		{ok, ConnPid} ->
			exit(ConnPid, "delete conn");
		error ->
			?ERROR("can not find cid for delete:~p",[Cid])
	end,			
	{reply, ok, State};
	
handle_call({get_conn_pid, Cid}, _From, #state{map_cid_pid = MapCP} = State) ->
	{reply, dict:find(Cid, MapCP), State};		
	
handle_call(Req, _From, State) ->
    ?WARNING("unexpect request: ~p", [Req]),
    {reply, {error, {invalid_request, Req}}, State}.
	
handle_cast({sync, Cid}, #state{map_cid_pid = MapCP} = State) ->
	case dict:find(Cid, MapCP) of
		{ok, Conn} ->
			extend104_connection:send(Conn, 'C_IC_NA_1'),
			extend104_connection:send(Conn, 'C_CI_NA_1'),
			extend104_connection:send(Conn, 'C_CS_NA_1');
		error ->
			?ERROR("can not sync,no conn:~p", [Cid])
	end,			
	{noreply, State};
	
handle_cast(_Msg, State) ->
	{noreply, State}.
	
	
handle_info({status, Cid, Connect} = Payload, #state{channel = Channel} = State) ->
	amqp:send(Channel, <<"monitor.reply">>, term_to_binary(Payload)),
    {noreply, State};		
	
handle_info(Msg ,State) ->
	?ERROR("unext msg:~p", [Msg]),
	{noreply, State}.
				

terminate(_Reason, #state{cityid=CityId, channel=Channel}) ->
    ?ERROR("terminate : ~p", [{CityId, node()}]),
    amqp:send(Channel, <<"node.reply">>, term_to_binary({delete, {node_id, CityId, node()}})),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

get_cid(ConnConf) ->
	Cid = proplists:get_value(id, ConnConf),
	Cid.
		
	
	
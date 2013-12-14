-module(extend104).

-include("extend104.hrl").

-include_lib("elog/include/elog.hrl").

-export([start_link/2,
		open_conn/1,
		get_conn_pid/1
		]).

-behavior(gen_server).

-export([init/1,
		handle_call/3,
		handle_cast/2,
        handle_info/2,
        code_change/3,
        terminate/2]).


-record(state, {cityid, channel, connection_sup, map_oid_pid = dict:new(), map_cid_pid = dict:new()}).

start_link(CityId, ConnectionSup) ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [CityId, ConnectionSup], []).
	
open_conn(ConnConf) ->
	gen_server:call(?MODULE, {open_conn, ConnConf}).	
	
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
	
handle_connect(ConnConf, #state{connection_sup = ConnSup, map_oid_pid=MapOP, map_cid_pid=MapCP} = State) ->
    case extend104_connection_sup:start_connection(ConnSup, ConnConf) of
        {ok, ConnPid} ->
            Oid = get_oid(ConnConf), 
			Cid = get_cid(ConnConf),
			{reply, {ok, ConnPid}, State#state{map_oid_pid=dict:store(Oid, ConnPid, MapOP), 
					map_cid_pid=dict:store(Cid, ConnPid, MapCP)}};
        {error, Error} ->
            ?ERROR("get conn error: ~p, ~p", [Error, ConnConf]),
			{reply, {error, Error}, State}
     end.	
	
	
handle_call({open_conn, ConnConf}, _From, State) ->
	handle_connect(ConnConf, State);	
handle_call({get_conn_pid, Oid}, _From, #state{map_oid_pid = MapOP} = State) when is_record(Oid, extend104_oid)->
	{reply, dict:find(Oid, MapOP), State};	
handle_call({get_conn_pid, Cid}, _From, #state{map_cid_pid = MapCP} = State) ->
	{reply, dict:find(Cid, MapCP), State};		
handle_call(Req, _From, State) ->
    ?WARNING("unexpect request: ~p", [Req]),
    {reply, {error, {invalid_request, Req}}, State}.
	
handle_cast(_Msg, State) ->
	{noreply, State}.
	
handle_info(_Msg ,State) ->
	{noreply, State}.
				

terminate(_Reason, #state{cityid=CityId, channel=Channel}) ->
    ?ERROR("terminate : ~p", [{CityId, node()}]),
    amqp:send(Channel, <<"node.reply">>, term_to_binary({delete, {node_id, CityId, node()}})),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

	
get_oid(ConnConf) ->
	Ip = proplists:get_value(ip, ConnConf),	
	Port = proplists:get_value(port, ConnConf),
	#extend104_oid{ip=extbif:to_binary(Ip), port=Port}.
	
get_cid(ConnConf) ->
	Cid = proplists:get_value(id, ConnConf),
	Cid.
		
	
	
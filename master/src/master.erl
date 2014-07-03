%%%----------------------------------------------------------------------
%%% Created	: 2013-12-4
%%% author 	: hejin1026@gmail.com
%%%----------------------------------------------------------------------
-module(master).

-include_lib("elog/include/elog.hrl").

-behavior(gen_server).

-export([start_link/1,
		config/1, ertdb/1,
		get_queue/2]).

-export([init/1, 
		handle_call/3, 
		handle_cast/2, 
		handle_info/2, 
		terminate/2, 
		code_change/3]).

-record(state, {channel, ertdb, ertdb_config}).	

-import(extbif, [to_list/1]).
		
start_link(Config) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Config], []).
		
config(Cmd) ->
	gen_server:call(?MODULE, {config, Cmd}).		
	
ertdb(connect) ->
	gen_server:call(?MODULE, ertdb_connect);
ertdb(close) ->
	gen_server:call(?MODULE, ertdb_close).				
			
init([Config]) ->
    {ok, Conn} = amqp:connect(),
    Channel = open(Conn),
    ets:new(node_id, [bag, named_table]),
    io:format("~n~s:  manager is starting...[done]~n", [node()]),
	
    {ok, #state{channel = Channel, ertdb_config = Config}}.


open(Conn) ->
	{ok, Channel} = amqp:open_channel(Conn),
    amqp:topic(Channel, <<"master.topic">>),
    amqp:queue(Channel, <<"node.reply">>),
	amqp:queue(Channel, <<"agent.reply">>),
    amqp:consume(Channel, <<"node.reply">>),
	amqp:consume(Channel, <<"agent.reply">>),
    Channel.
	
handle_call(ertdb_connect, _From, #state{ertdb_config = Config}=State) ->
	case ertdb_client:start_link(Config) of
		{ok, Client} ->
			{reply, ok, State#state{ertdb=Client}};
		{error, Reason} ->
			{reply, {error, Reason}, State}
	end;		
	
handle_call(ertdb_close, _From, #state{ertdb = Client}=State) ->
	ertdb_client:stop(Client),
	{reply, ok, State#state{ertdb=undefined}};	
	
handle_call({config, _Cmd}, _From, #state{ertdb=undefined}=State) ->	
	{reply, {error, unconn}, State};
	
handle_call({config, Cmd}, _From, #state{ertdb=Client}=State) ->
	case ertdb_client:q(Client, Cmd) of
		{ok, <<"1">>} ->
			ok;
		{error, Reason} ->
			?ERROR("config error:~p,~p", [Cmd, Reason])	
	end,		
	{reply, ok, State};

handle_call(Req, _From, State) ->
    ?ERROR("Unexpected request: ~p", [Req]),
    {reply, ok, State}.


handle_cast(Msg, State) ->
    ?ERROR("Unexpected message: ~p", [Msg]),
    {noreply, State}.


handle_info({deliver, <<"node.reply">>, _Properties, Payload}, State) ->
    ?ERROR("get reply :~p", [binary_to_term(Payload)]),
    handle_nodeid(binary_to_term(Payload)),
	{noreply, State};

handle_info({deliver, <<"agent.reply">>, _Properties, Payload}, State) ->
    % ?ERROR("get reply :~p", [binary_to_term(Payload)]),
    handle_agent(binary_to_term(Payload)),
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
handle_nodeid({add, {Type, CityId, Node}}) ->
    ets:insert(node_id, {{Type, CityId}, Node});
handle_nodeid({delete, {Type, CityId, Node}}) ->
    ets:delete_object(node_id, {{Type, CityId}, Node});
handle_nodeid(Other) ->
    ?ERROR("Unexpected node_id: ~p", [Other]).
	
	
get_queue(monitor, CityId) ->
    case get_one_nodeid({node_id, to_list(CityId)}) of
		false ->
			false;
		NodeId ->
			NodeId ++ ".monitor"
	end.		

get_one_nodeid({Type, CityId}) ->
    NodeIds = get_all_nodeid({Type, CityId}),
    case NodeIds of
        [] ->
            ?ERROR("no node id ~p", [{Type, CityId}]), 
			false;
        _ ->
            {{Type, Id}, _} = lists:nth(random:uniform(length(NodeIds)), NodeIds),
            Id
    end.		
	
%cityid : list
get_all_nodeid({Type, CityId}) ->
    NodeId = ets:lookup(node_id, {Type, CityId}),
    NodeId2 = ets:lookup(node_id, {Type, "common"}),
    NodeId ++ NodeId2.

get_one_common(Type) ->
    case ets:lookup(node_id, {Type, "common"}) of
        [] -> [];
        _ -> ["common"]
     end.
get_one_group(Type, CityId) ->
    case ets:lookup(node_id, {Type, CityId}) of
        [] -> [];
        _ -> [CityId]
     end.



handle_agent({hostinfo, HostInfo}) ->
    DateTime = {datetime, {date(), time()}},
    SID = proplists:get_value(sid, HostInfo),
    case emysql:select({servers, {sid, SID}}) of
        {ok, [_Record|_]} ->
            case emysql:update(servers, [{updated_at, DateTime} | HostInfo], {sid, SID}) of
                {error, Reason} ->
                    ?ERROR("insert host  :~p, ~n Reason: ~p", [HostInfo, Reason]);
                _ ->
                    ok
             end;
        {ok, []} ->
            case emysql:insert(servers, [{created_at, DateTime} | HostInfo]) of
                {error, Reason} ->
                    ?ERROR("insert host  :~p, ~n Reason: ~p", [HostInfo, Reason]);
                _ ->
                    ok
            end;
        {error, Reason} ->
            ?ERROR("~p",[Reason])
    end.
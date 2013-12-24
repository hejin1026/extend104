%%%----------------------------------------------------------------------
%%% Created	: 2013-12-4
%%% author 	: hejin1026@gmail.com
%%%----------------------------------------------------------------------
-module(master).

-include_lib("elog/include/elog.hrl").

-behavior(gen_server).

-export([start_link/0,
		get_queue/2]).

-export([init/1, 
		handle_call/3, 
		handle_cast/2, 
		handle_info/2, 
		terminate/2, 
		code_change/3]).

-record(state, {channel}).	

-import(extbif, [to_list/1]).
		
start_link() ->
    gen_server2:start_link({local, ?MODULE}, ?MODULE, [], []).
		
			
init([]) ->
    {ok, Conn} = amqp:connect(),
    Channel = open(Conn),
    ets:new(node_id, [bag, named_table]),
    io:format("~n~s:  manager is starting...[done]~n", [node()]),
    {ok, #state{channel = Channel}}.


open(Conn) ->
	{ok, Channel} = amqp:open_channel(Conn),
    amqp:topic(Channel, <<"master.topic">>),
    amqp:queue(Channel, <<"node.reply">>),
    amqp:consume(Channel, <<"node.reply">>),
    Channel.

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
    NodeId = get_one_nodeid({node_id, to_list(CityId)}),
    NodeId ++ ".monitor".	

get_one_nodeid({Type, CityId}) ->
    NodeIds = get_all_nodeid({Type, CityId}),
    case NodeIds of
        [] ->
            ?ERROR("no node id ~p", [{Type, CityId}]), [];
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


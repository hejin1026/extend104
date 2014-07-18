%%%----------------------------------------------------------------------
%%% Created	: 2014-7-11
%%% author 	: hejin1026@gmail.com
%%%----------------------------------------------------------------------
-module(pavilion).

-behavior(gen_server).

-include_lib("elog/include/elog.hrl").

-export([start_link/0,
		subscribe/2,
		unsubscribe/2
		]).

-export([init/1, 
		handle_call/3, 
		handle_cast/2, 
		handle_info/2, 
		terminate/2, 
		code_change/3]).

-record(state, {channel}).

-import(extbif, [to_binary/1]).
	
subscribe(Cid, WebSocket) ->
	gen_server2:call(?MODULE, {subscribe, Cid, WebSocket}).	
	
unsubscribe(Cid, WebSocket) ->
	gen_server2:call(?MODULE, {unsubscribe, Cid, WebSocket}).		
	
start_link() ->
    gen_server2:start_link({local, ?MODULE}, ?MODULE, [], []).
		
init([]) ->
    {ok, Conn} = amqp:connect(),
    Channel = open(Conn),
	ets:new(cid_wb, [bag, named_table]),	
    {ok, #state{channel = Channel}}.

open(Conn) ->
    {ok, Channel} = amqp:open_channel(Conn),
    amqp:queue(Channel, <<"pavilion.reply">>),
    amqp:consume(Channel, <<"pavilion.reply">>),
    Channel.
	
handle_call({subscribe, Cid, WebSocket}, _From, #state{channel = Channel}=State) ->
	Reply = handle_subscribe(Cid, WebSocket),				
	{reply, Reply, State};		

handle_call({unsubscribe, Cid, WebSocket}, _From, #state{channel = Channel}=State) ->
    Reply = case lookup(Cid) of
        [] ->
            {error, no_subscribe};
        [{Cid, WebSocket}] ->
			handle_unsubscribe([Cid, WebSocket]);		
		_WSPid ->
			ets:delete_object(cid_wb, {Cid, WebSocket})	
    end,
	{reply, Reply, State};	
		
	
handle_call(Msg, _From, State) ->
	?ERROR("unext call msg :~p", [Msg]),
	{reply, ok, State}.
	
handle_cast(Msg, State) ->
	?ERROR("unext case msg :~p", [Msg]),
	{noreply, State}.
	
	
handle_info({deliver, <<"pavilion.reply">>, _Properties, Payload}, State) ->
    ?INFO("get pavilion reply :~p", [binary_to_term(Payload)]),
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
handle_subscribe(Cid, WebSocket) ->
    case get_master() of
		 false ->
			 {error, nomaster};
		 Node ->
			case rpc:call(Node, master_dist, subscribe, [Cid, WebSocket]) of
					 {badrpc, Reason} ->
						 {error, badrpc};
					 {error, Reason} ->
						 {error, Reason};	 
					 true ->
						ets:insert(cid_wb, {Cid, WebSocket})
			end
	end.
	
handle_unsubscribe(Args) ->
	case get_master() of
		 false ->
			 {error, nomaster};
		 Node ->	 
			case rpc:call(Node, master_dist, unsubscribe, Args) of
				 {badrpc, Reason} ->
					 {error, badrpc};
				 {error, Reason} ->
					 {error, Reason};	 
				 true ->	 
				 	[Cid|_] = Args,
					ets:delete(cid_wb, Cid) 			
			end
	end.

handle_reply({frame, Cid, {Type, Time, Frame}}, #state{channel=Channel}) ->
	case lookup(Cid) of
		[] ->
			handle_unsubscribe([Cid]);
		WSPid ->
			[Pid ! {frame, Type, Time, Frame} || {_, Pid} <- WSPid]
	end;	
	
handle_reply({status, Cid, Time, Connect}, #state{channel=Channel}) ->
	case lookup(Cid) of
		[] ->
			ignore;
		WSPid ->
			[Pid ! {status, Cid, Time, Connect} || {_, Pid} <- WSPid]
	end;				
				
handle_reply(_Reply, _State) ->
    ok.		

lookup(Cid) ->
	ets:lookup(cid_wb, Cid). 
	
get_master() ->
	case lists:filter(fun(SNode) ->
			NodeName = case string:tokens(atom_to_list(SNode), "@") of
					[Node, _Server] ->
					    Node;
					_ ->
						SNode
			end,				
			NodeName == "master"
		end, nodes())  of
		[] ->
			false;
		[SNode|_] ->
			SNode
	end.		
				
		
		
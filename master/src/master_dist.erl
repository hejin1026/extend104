%%%----------------------------------------------------------------------
%%% Created	: 2013-12-4
%%% author 	: hejin1026@gmail.com
%%%----------------------------------------------------------------------
-module(master_dist).

-behavior(gen_server).

-include_lib("elog/include/elog.hrl").

-export([start_link/0,
		dispatch/1
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
	
start_link() ->
    gen_server2:start_link({local, ?MODULE}, ?MODULE, [], []).
		
init([]) ->
    {ok, Conn} = amqp:connect(),
    Channel = open(Conn),
    mnesia:create_table(dispatch, [{ram_copies, [node()]},
        {attributes, record_info(fields, dispatch)}]),
    {ok, #state{channel = Channel}}.

open(Conn) ->
    {ok, Channel} = amqp:open_channel(Conn),
    amqp:queue(Channel, <<"monitor.reply">>),
    amqp:consume(Channel, <<"monitor.reply">>, self()),
    Channel.
		

handle_call(Msg, _From, State) ->
	{reply, ok, State}.
	
handle_cast({dispatch, {monitor, Cid, Data}=Payload}, #state{channel = Channel} = State) ->
    CityId = proplists:get_value(cityid, Data),
    Payload2 = term_to_binary(Payload),
	NodeId = master:get_queue(monitor, CityId),
	?INFO("get nodeid:~p", [NodeId]),
    case mnesia:dirty_read(dispatch, {monitor, Cid}) of
        [] ->
            amqp:send(Channel, to_binary(NodeId), Payload2),
            mnesia:dirty_write(#dispatch{id = {monitor, Cid}});
        [#dispatch{node = undefined}] ->
            amqp:send(Channel, to_binary(NodeId), Payload2);
        [#dispatch{node = Node}] ->
            amqp:send(Channel, Node, Payload2)
    end,
	{noreply, State};	
	
handle_cast(Msg, State) ->
	{noreply, State}.
	
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

handle_reply({monitored, Cid, Node}, _State) ->
    case mnesia:dirty_read(dispatch, {monitor, Cid}) of
        [#dispatch{node = OldNode} = Dispatch] ->
            if
                OldNode == undefined -> ok;
                OldNode == Node -> ok;
                true -> ?ERROR("tow nodes for one dn: ~p, oldnode: ~p, newnode: ~p", [Cid, OldNode, Node])
            end,
            mnesia:dirty_write(Dispatch#dispatch{node = Node});
        [] ->
            ?ERROR("unexpected reply: ~p", [{monitored, Cid}])
    end;
handle_reply(_Reply, _State) ->
    ok.		
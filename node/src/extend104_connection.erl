-module(extend104_connection).

-include("extend104.hrl").

-include("extend104_frame.hrl").

-include_lib("elog/include/elog.hrl").

-export([start_link/2,
		status/1,
		send/2,
		subscribe/2,
		unsubscribe/2
		]).

-behavior(gen_fsm).

%% gen_fsm callbacks
-export([init/1,
        handle_info/3,
        handle_event/3,
        handle_sync_event/4,
        code_change/4,
        terminate/3]).

% fsm state
-export([connecting/2,
        connecting/3,
        connected/2,
        connected/3,
		disconnected/2]).

-define(TCPOPTIONS, [binary, {packet, raw}, {active, true}]).
		%{backlog,   128},
		%{nodelay,   true},
		%{reuseaddr, true}]).

-define(TIMEOUT, 8000).

-record(state, {server, cid, host, port, sock, rest= <<>>, subscriber = [], timer}).

-import(extbif, [to_list/1]).

start_link(Server, Args) ->
	gen_fsm:start_link(?MODULE, [Server, Args], []).

status(C) when is_pid(C) ->
	gen_fsm:sync_send_all_state_event(C, status).	

-spec send(C :: pid(), Cmd :: 'STARTDT' | 'STOPDT' | 'C_IC_NA_1' | atom()) -> ok.
send(C, Cmd) when is_pid(C) and is_atom(Cmd) ->
	gen_fsm:send_event(C, Cmd).
	
subscribe(C, SPid) ->
	gen_fsm:sync_send_event(C, {subscribe,SPid}).	
	
unsubscribe(C, SPid) ->
	gen_fsm:sync_send_event(C, {unsubscribe,SPid}).		


init([Server, Args]) ->
	?INFO("conn info:~p", [Args]),
	Cid = proplists:get_value(id, Args),
	Host = proplists:get_value(ip, Args),
	Port = proplists:get_value(port, Args),
	put(recv_c,0),
	put(send_c,0),
	put(ser_send_cn,{0,0}),
	put(ser_recv_cn,{0,0}),
	{ok, connecting, #state{server=Server, cid=Cid, host=to_list(Host), port=Port}, 0}.

connecting(timeout, State) ->
    connect(State);
connecting(_Event, State) ->
    {next_state, connecting, State}.
	
connecting(_Event, _From, State) ->
    {reply, {error, connecting}, connecting, State}.

connect(State = #state{server=Server, cid=Cid, host=Host, port=Port}) ->
	?INFO("begin to conn", []),
    case gen_tcp:connect(Host, Port, ?TCPOPTIONS, ?TIMEOUT) of 
    {ok, Sock} ->
		?INFO("~p:~p is connected.", [Host, Port]),
		send(self(), 'STARTDT'),
		Server ! {status, Cid, connected},
        {next_state, connected, State#state{sock = Sock}};
    {error, Reason} ->
		?ERROR("failed to connect ~p:~p, error: ~p.", [Host, Port, Reason]),
		erlang:send_after(30000, self(), reconnect),
        {next_state, connecting, State#state{sock = undefined}}
    end.


% 启动
connected('STARTDT', State) ->
	send_frame(?FRAME_STARTDT, State),
	{next_state, connected, State};
% 停止
connected('STOPDT', State) ->
	send_frame(?FRAME_STOPDT, State),
	{next_state, connected, State};
% 总召 100
connected('C_IC_NA_1', #state{cid=Cid}=State) ->
	BCid = extend104_util:reverse_int_value(Cid),
	send_frame(?FRAME_C_IC_NA_1(BCid), State),
	{next_state, connected, State};
% 计量总召 101
connected('C_CI_NA_1', #state{cid=Cid}=State) ->
	BCid = extend104_util:reverse_int_value(Cid),
	send_frame(?FRAME_C_CI_NA_1(BCid), State),
	{next_state, connected, State};
% 时钟同步 103
connected('C_CS_NA_1', #state{cid=Cid}=State) ->
	BCid = extend104_util:reverse_int_value(Cid),
	send_frame(?FRAME_C_CS_NA_1(BCid), State),
	{next_state, connected, State};
connected(Frame, State) ->
	?ERROR("badevent: ~p", [Frame]),
	{next_state, connected, State}.
	
	
connected({subscribe, SPid}, _From, #state{subscriber=Subs}=State) ->
	{reply, ok, connected, State#state{subscriber=[SPid|Subs]}};	
connected({unsubscribe, SPid}, _From, #state{subscriber=Subs}=State) ->
	{reply, ok, connected, State#state{subscriber=[SP||SP <- Subs, SP =/= SPid]}};		
connected(_Event, _From, State) ->
    {reply, {error, badevent}, connected, State}.

disconnected(connect, State) ->
	connect(State);
disconnected(Event, State) ->
	?ERROR("badevent ~p", [Event]),
	{next_state, discconnected, State}.


handle_info({tcp, _Sock, Data}, connected, #state{rest=Rest, timer=LastTimer}=State) ->
	?INFO("get msg lenth:~p, ~p",[size(Data), Data]),
	NewRest = check_frame(list_to_binary([Rest, Data]), State),
	if LastTimer == undefined -> ok;
		true -> erlang:cancel_timer(LastTimer)
	end,	
	Timer = erlang:start_timer(20000, self(), heartbeat),
    {next_state, connected, State#state{rest=NewRest, timer = Timer}};

handle_info({tcp_closed, Sock}, connected, State=#state{sock=Sock}) ->
	?ERROR_MSG("tcp closed."),
    {next_state, disconnected, State};

handle_info(reconnect, connecting, S) ->
    connect(S);

handle_info({timeout, _Timer, heartbeat}, connected, State) ->
	send_frame(?FRAME_TESTFR_SEND, State),
    {next_state, connected, State};

handle_info(_Info, StateName, State) ->
    {next_state, StateName, State}.

handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.

handle_sync_event(status, _From, StateName, State) ->
    {reply, StateName, StateName, State};

handle_sync_event(stop, _From, _StateName, State) ->
    {stop, normal, ok, State}.

terminate(_Reason, _StateName, _State) ->
    ok.

code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

%%---------------- fun ------------------%%	
	
check_frame(<<>>, _State) -> <<>>;		
check_frame(<<16#68,L,Data/binary>> = Frames, State) ->
	if L =< size(Data) ->
		{FrameData, Rest} = split_binary(Data, L),
		recv_frame(FrameData, State),
		check_frame(Rest, State);	
	true ->
		Frames
	end;		
check_frame(Rest, _State) ->
	?ERROR("get error msg:~p",[Rest]), 
	<<>>.		
	
recv_frame(FrameData, #state{cid=Cid, subscriber=Subs}=State) ->
	Frame = extend104_frame:parse(FrameData),
	[SPid ! {frame, Cid, {recv, calendar:local_time(), Frame} } ||SPid <- Subs],
	process_frame(Frame, State).	

% send_cn is current = last s_recv_cn, recv_cn is next = last s_send_cn + 1
send_frame(Frame, State) ->
	SendFrame = 
		case Frame of
			?FRAME_STARTDT -> Frame;
			?FRAME_STOPDT -> Frame;
			?FRAME_TESTFR_SEND -> Frame;
			?FRAME_TESTFR_REPLY -> Frame;
			#extend104_frame{c1=1,c2=0} -> Frame;
			_ ->
				put_c(send_c, get(send_c) + 1),
				{RC1,RC2} = extend104_util:add_cn(get(ser_send_cn)),
				{SC1,SC2} = get(ser_recv_cn),
				?INFO("send_c:~p, send_cn:~p", [{get(send_c), count_to_curr_c(get(send_c))}, {SC1,SC2}]),
				Frame#extend104_frame{c1=SC1,c2=SC2,c3=RC1,c4=RC2}
		end,	
	do_send_frame(SendFrame, State).
	
do_send_frame(Frame, #state{cid=Cid, sock = Sock,subscriber=Subs}) when is_record(Frame, extend104_frame) ->
	?INFO("send msg:~p", [extend104_frame:serialise(Frame)]),
	[SPid ! {frame, Cid, {send, calendar:local_time(), Frame}} ||SPid <- Subs],
    erlang:port_command(Sock, extend104_frame:serialise(Frame)).
		

process_frame(#extend104_frame{c1 = C1} = Frame, State) ->
	?INFO("Received: ~p", [Frame]),
	Tag1 = C1 band 16#01,	
	Tag2 = C1 band 16#03,
	if
	Tag1 == 0 -> 
		process_apci_i(Frame, State);
	Tag2 == 1 ->
		process_apci_s(Frame);
	Tag2 == 3 ->
		process_apci_u(Frame, State);
	true ->
		?ERROR("unsupport apci....~p", [Frame])
	end.

process_apci_i(Frame, State) ->
	put(ser_send_cn, {Frame#extend104_frame.c1, Frame#extend104_frame.c2}),	
	put(ser_recv_cn, {Frame#extend104_frame.c3, Frame#extend104_frame.c4}),
	confirm_frame(State),
	DateTime = {datetime, {date(), time()}},
	case extend104_frame:process_asdu(Frame) of
		ok -> ok;
		{measure, DataList} ->
			handle_data({measure, DateTime, DataList}, State)
	end.		

handle_data({measure, DateTime, DataList}, #state{cid=Cid}) ->
	?INFO("get measure from cid:~p",[Cid]),
	extend104_hub:send_datalog({measure, Cid, DateTime, DataList}).
	
			
process_apci_s(Frame) ->
	?INFO("get apci_s :~p",[Frame]).

process_apci_u(?FRAME_TESTFR_SEND, State) ->
	send_frame(?FRAME_TESTFR_REPLY, State);
process_apci_u(?FRAME_TESTFR_REPLY, _State) ->
	ok;	
process_apci_u(Frame, _State) -> %<<TESTFR:2,STOPDT:2,STARTDT:2,_:2>>
	% STOPDT = (C1 bsr 4) band 2#11,	
	% STARTDT = (C1 bsr 2) band 2#11,
	?INFO("get apci_u:~p",[Frame]).

% special send
confirm_frame(State) ->
	RC = get(recv_c) + 1,
	if (RC rem 8) == 0 ->
		RCn = count_to_next_c(RC),
		ExpRecvCn = {L,H} = extend104_util:add_cn(get(ser_send_cn)),
		FrameConfirm = ?FRAME_CONFIRM#extend104_frame{c3=L, c4=H},
		?INFO("confirm recv_c:~p, exp_recv_cn:~p", [{RC, RCn}, ExpRecvCn]),
		send_frame(FrameConfirm, State);
	true ->
		ok
	end,		
	put_c(recv_c, RC).	
	
	
% begin with get->0, put->0	
put_c(Type, Value) ->
	V = if Value > 32767 -> 0;
		true -> Value
		end,
	put(Type, V).	
	
% for current cn (1 -> 0,0| 2 -> 2,0| 3 -> 4,0)	
count_to_curr_c(C) ->
	count_to_next_c(C-1).	
% for next cn [recv|send] (1 -> 2,0| 2 -> 4,0| 3 -> 6,0)	
count_to_next_c(C) ->
	C3 = (C band 16#FF) bsl 1 ,
	C4 = (C bsr 8),	
	{C3, C4}.
	
	
-module(extend104).

-include("extend104.hrl").

-include("extend104_frame.hrl").

-include_lib("elog/include/elog.hrl").

-export([start_link/1,
		status/1,
		send/2]).

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

-record(state, {host, port, sock,rest= <<>>, msgid}).

start_link(Args) ->
	gen_fsm:start_link(?MODULE, [Args], []).

status(C) when is_pid(C) ->
	gen_fsm:sync_send_all_state_event(C, status).

-spec send(C :: pid(), Cmd :: 'STARTDT' | 'STOPDT' | 'C_IC_NA_1' | atom()) -> ok.
send(C, Cmd) when is_pid(C) and is_atom(Cmd) ->
	gen_fsm:send_event(C, Cmd).

init([Args]) ->
	?INFO("~p", [Args]),
	Host = proplists:get_value(host, Args),
	Port = proplists:get_value(port, Args),
	{ok, connecting, #state{host=Host, port=Port}, 0}.

connecting(timeout, State) ->
    connect(State);

connecting(_Event, State) ->
    {next_state, connecting, State}.

connecting(_Event, _From, State) ->
    {reply, {error, connecting}, connecting, State}.

connect(State = #state{host=Host, port=Port}) ->
    case gen_tcp:connect(Host, Port, ?TCPOPTIONS) of %, ?TIMEOUT) of
    {ok, Sock} ->
		?INFO("~p:~p is connected.", [Host, Port]),
        {next_state, connected, State#state{sock = Sock}};
    {error, Reason} ->
		?ERROR("failed to connect ~p:~p, error: ~p.", [Host, Port, Reason]),
		erlang:send_after(30000, self(), reconnect),
        {next_state, connecting, State#state{sock = undefined}}
    end.

% 启动
connected('STARTDT', State=#state{sock = Sock}) ->
	send_frame(Sock, ?FRAME_STARTDT),
	{next_state, connected, State};

% 停止
connected('STOPDT', State=#state{sock = Sock}) ->
	send_frame(Sock, ?FRAME_STOPDT),
	{next_state, connected, State};

% 总召 100
connected('C_IC_NA_1', State=#state{sock = Sock}) ->
	send_frame(Sock, ?FRAME_C_IC_NA_1),
	{next_state, connected, State};

% 计量总召 101
connected('C_CI_NA_1', State=#state{sock = Sock}) ->
	send_frame(Sock, ?FRAME_C_CI_NA_1),
	{next_state, connected, State};

% 时钟同步 103
connected('C_CS_NA_1', State=#state{sock = Sock}) ->
	send_frame(Sock, ?FRAME_C_CS_NA_1),
	{next_state, connected, State};

connected(Frame, State=#state{sock = Sock}) ->
	?ERROR("badevent: ~p", [Frame]),
	erlang:port_command(Sock,Frame),
	{next_state, connected, State}.

connected(_Event, _From, State) ->
    {reply, {error, badevent}, connected, State}.

disconnected(connect, State) ->
	connect(State);

disconnected(Event, State) ->
	?ERROR("badevent ~p", [Event]),
	{next_state, discconnected, State}.

handle_info({tcp, _Sock, Data}, connected, #state{sock=Sock,rest=Rest}=State) ->
	?INFO("get msg lenth:~p, ~p",[size(Data), Data]),
	ProcessFun = fun(Frame) ->
			process_frame(extend104_frame:parse(Frame), Sock)
		  end,	
	NewRest = extend104_frame:check_frame(list_to_binary([Rest, Data]), ProcessFun),
    {next_state, connected, State#state{rest=NewRest}};

handle_info({tcp_closed, Sock}, connected, State=#state{sock=Sock}) ->
	?ERROR_MSG("tcp closed."),
    {next_state, disconnected, State};

handle_info({timeout, reconnect}, connecting, S) ->
    connect(S);

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

send_frame(Sock, Frame) when is_binary(Frame) ->
    erlang:port_command(Sock, Frame);
	
send_frame(Sock, Frame) ->
	?INFO("send msg:~p", [extend104_frame:serialise(Frame)]),
    erlang:port_command(Sock, extend104_frame:serialise(Frame)).

process_frame(#extend104_frame{c1 = C1} = Frame, Sock) ->
	?INFO("Received: ~p", [Frame]),
	Tag1 = C1 band 16#01,	
	Tag2 = C1 band 16#03,
	if
	Tag1 == 0 -> % apci_i
		process_apci_i(Frame);
	Tag2 == 1 ->
		process_apci_s(Frame);
	Tag2 == 3 ->
		process_apci_u(Frame, Sock);
	true ->
		?ERROR("unsupport apci....~p", [Frame])
	end.

process_apci_i(Frame) ->
	process_asdu(Frame).
			
process_apci_s(#extend104_frame{c3 = C1} = Frame) ->
	<<LSB:7,0:1>> = <<C1>>,
	?INFO("get apci_s :~p",[Frame]),
	ok.

process_apci_u(?FRAME_TESTFR_R, Sock) ->
	send_frame(Sock, ?FRAME_TESTFR_T);
process_apci_u(#extend104_frame{c1 = C1}, Sock) -> %<<TESTFR:2,STOPDT:2,STARTDT:2,_:2>>
	STOPDT = (C1 bsr 4) band 2#11,
	STARTDT = (C1 bsr 2) band 2#11,
	?INFO("stopdt:~p,startdt:~p",[STOPDT,STARTDT]),
	ok.
	
process_asdu(Frame = #extend104_frame{payload = <<>>}) ->
	?ERROR("empty ~p", [Frame]);

process_asdu(#extend104_frame{payload = <<Type,SQ:1,VSQ:7,COT:8,_COT:1/binary,Addr:2/binary,Data/binary>>}) ->
	ASDU = #extend104_asdu{type=Type, sq=SQ, vsq=VSQ, cot=COT, addr=extend104_util:reverse_byte(Addr), data=Data},
	process_asdu(Type, ASDU).

%%---------------- fun ------------------%%

% 初始化结束
process_asdu(?M_EI_NA_1, _ASDU) ->
	?INFO_MSG("sub station reset!");
	
% 100:总召确认 7	
process_asdu(?C_IC_NA_1, #extend104_asdu{cot= ?M_COT_ACTCON_1} = ASDU) ->
	?INFO("all confirm :~p",[ASDU]);	
% 100:总召结束 10
process_asdu(?C_IC_NA_1, #extend104_asdu{cot= ?M_COT_ACTTERM_1} = ASDU) ->
	?INFO("all over :~p",[ASDU]);	

% 101:计算量总召确认 7	
process_asdu(?C_CI_NA_1, #extend104_asdu{cot= ?M_COT_ACTCON_1} = ASDU) ->
	?INFO("all count confirm :~p",[ASDU]);	
% 101:计算量总召结束 10	
process_asdu(?C_CI_NA_1, #extend104_asdu{cot= ?M_COT_ACTTERM_1} = ASDU) ->
	?INFO("all count over:~p",[ASDU]);				

% 103:时钟同步确认 7
process_asdu(?C_CS_NA_1, #extend104_asdu{cot= ?M_COT_ACTCON_1} = ASDU) ->
	?INFO("all time confirm :~p",[ASDU]);		

% 1:单点NA
process_asdu(?M_SP_NA_1, ASDU) ->
	?INFO_MSG("recv M_SP_NA_1 frame"),
	DataList = process_M_SP_NA(ASDU),
	?INFO("get 1 asdu:~p,data length:~p, ~n ~p", [ASDU#extend104_asdu{data= <<>>}, length(DataList), DataList]);

% 9:测量值--归一化值
process_asdu(?M_ME_NA_1, ASDU) ->
	process_M_ME_NA(ASDU);
% 11:测量值--标度化值，与归一化相同处理
process_asdu(?M_ME_NB_1, ASDU) ->
	DataList = process_M_ME_NA(ASDU),
	?INFO("get 11 asdu:~p,data length:~p, ~n ~p", [ASDU#extend104_asdu{data= <<>>}, length(DataList), DataList]);
	
% 15:电能脉冲计数量帧
process_asdu(?M_IT_NA_1, ASDU) -> 
	DataList = process_M_IT_NA(ASDU),
	?INFO("get 15 asdu:~p,data length:~p, ~n ~p", [ASDU#extend104_asdu{data= <<>>}, length(DataList), DataList]);

process_asdu(Type, Payload) ->
	?ERROR("Unexepected ASDU: {~p, ~p}", [Type, Payload]).





% 1：M_SP_NA_1 遥信
process_M_SP_NA(#extend104_asdu{sq=0, data=Data}) ->
	process_M_SP_NA_0(Data, []);
process_M_SP_NA(#extend104_asdu{sq=1, data = <<PAddr:3/binary,Other/binary>>}) ->
	RPAddr = extend104_util:reverse_byte(PAddr),	
	[{RPAddr, process_M_SP_NA_1(Other, [])}].

process_M_SP_NA_0(<<>>, Acc) ->
	Acc;
process_M_SP_NA_0(<<PAddr:3/binary, IV:1, NT:1, SB:1,BL:1,SPI:4,Other/binary>>, Acc) ->
	RPAddr = extend104_util:reverse_byte(PAddr),	
	% ?INFO("get addr:~p,info :~p", [RPAddr, {IV,NT,SB,BL,SPI}]),
	process_M_SP_NA_0(Other, [{RPAddr, {IV,NT,SB,BL,SPI}}|Acc]).
			
process_M_SP_NA_1(<<>>, Acc) ->	
	Acc;	
process_M_SP_NA_1(<<IV:1, NT:1, SB:1,BL:1,SPI:4,Other/binary>>, Acc) ->
	% ?INFO("get info :~p", [{IV,NT,SB,BL,SPI}]),
	process_M_SP_NA_1(Other, [{IV,NT,SB,BL,SPI}|Acc]).


% 11：M_ME_NA_1 遥测
process_M_ME_NA(#extend104_asdu{sq=0, data=Data}) ->
	process_M_ME_NA_0(Data, []);
process_M_ME_NA(#extend104_asdu{sq=1, data = <<PAddr:3/binary,Other/binary>>}) ->
	RPAddr = extend104_util:reverse_byte(PAddr),	
	[{RPAddr, process_M_ME_NA_1(Other, [])}].
	
process_M_ME_NA_0(<<>>, Acc) ->
	Acc;
process_M_ME_NA_0(<<PAddr:3/binary, Value:2/binary, IV:1, NT:1, SB:1,BL:1,OV:4,Other/binary>>, Acc) ->
	RPAddr = extend104_util:reverse_byte(PAddr),	
	% ?INFO("get addr:~p,info :~p", [RPAddr, {Value,IV,NT,SB,BL,OV}]),
	process_M_ME_NA_0(Other, [{RPAddr, {Value,IV,NT,SB,BL,OV}}|Acc]).	
	
process_M_ME_NA_1(<<>>, Acc) ->
	Acc;		
process_M_ME_NA_1(<<Value:2/binary, IV:1, NT:1, SB:1,BL:1,OV:4,Other/binary>>, Acc) ->
	% ?INFO("get info :~p", [{Value,IV,NT,SB,BL,OV}]),
	process_M_ME_NA_1(Other, [{Value,IV,NT,SB,BL,OV}|Acc]).

% 15：M_IT_NA_1	计数量
process_M_IT_NA(#extend104_asdu{sq=0, data=Data}) ->
	process_M_IT_NA_0(Data, []);
process_M_IT_NA(#extend104_asdu{sq=1, data = <<PAddr:3/binary,Other/binary>>}) ->
	RPAddr = extend104_util:reverse_byte(PAddr),	
	[{RPAddr, process_M_IT_NA_1(Other, [])}].
	
process_M_IT_NA_0(<<>>, Acc) ->
	Acc;
process_M_IT_NA_0(<<PAddr:3/binary, Value:4/binary, IV:1, CA:1, CY:1,SQ:5,Other/binary>>, Acc) ->
	RPAddr = extend104_util:reverse_byte(PAddr),	
	% ?INFO("get addr:~p,info :~p", [RPAddr, {Value,IV,CA,CY,SQ}]),
	process_M_IT_NA_0(Other, [{RPAddr, {Value,IV,CA,CY,SQ}}|Acc]).	
	
process_M_IT_NA_1(<<>>, Acc) ->
	Acc;		
process_M_IT_NA_1(<<Value:4/binary, IV:1, CA:1, CY:1,SQ:5,Other/binary>>, Acc) ->
	% ?INFO("get info :~p", [{Value,IV,CA,CY,SQ}]),
	process_M_IT_NA_1(Other, [{Value,IV,CA,CY,SQ}|Acc]).	
	
	
	
	
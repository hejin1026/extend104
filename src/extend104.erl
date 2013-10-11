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

-record(state, {host, port, sock, msgid}).

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

connected('STARTDT', State=#state{sock = Sock}) ->
	send_frame(Sock, ?FRAME_STARTDT),
	{next_state, connected, State};

connected('STOPDT', State=#state{sock = Sock}) ->
	send_frame(Sock, ?FRAME_STOPDT),
	{next_state, connected, State};

connected('C_IC_NA_1', State=#state{sock = Sock}) ->
	send_frame(Sock, ?FRAME_C_IC_NA_1),
	{next_state, connected, State};

connected(Event, State) ->
	?ERROR("badevent: ~p", [Event]),
	{next_state, connected, State}.

connected(_Event, _From, State) ->
    {reply, {error, badevent}, connected, State}.

disconnected(connect, State) ->
	connect(State);

disconnected(Event, State) ->
	?ERROR("badevent ~p", [Event]),
	{next_state, discconnected, State}.

handle_info({tcp, _Sock, Data}, connected, State) ->
	case extend104_frame:parse(Data) of
	{ok, Frame} -> 
		process_frame(Frame);
	{error, Error} -> 
		?ERROR("~p", [Error])
	end,
    {next_state, connected, State};

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
    erlang:port_command(Sock, extend104_frame:serialise(Frame)).

process_frame(#extend104_frame{c1 = C1} = Frame) ->
	?INFO("Received: ~p", [Frame]),
	Tag1 = C1 band 16#01,	
	Tag2 = C1 band 16#03,
	if
	Tag1 == 0 ->
		process_apci_i(Frame);
	Tag2 == 1 ->
		process_apci_s(Frame);
	Tag2 == 3 ->
		process_apci_u(Frame);
	true ->
		process_asdu(Frame)
	end.
	%m_pChannel->SaveEnd(0, IDLE_TEST);
	

process_apci_i(Frame) ->
	%TODO: return m_pCtlInfo->DealwithAPCI_I(pAPCI->wTxSeq, pAPCI->wRxSeq);
	ok.
	
process_apci_s(Frame) ->
	%TODO: return m_pCtlInfo->DealwithAPCI_S(pAPCI->wRxSeq);
	ok.
	

process_apci_u(Frame) ->
	%TODO: return m_pCtlInfo->DealwithAPCI_U(pAPCI->byInfo[0]);
	ok.
	
process_asdu(Frame = #extend104_frame{payload = <<>>}) ->
	?ERROR("empty ~p", [Frame]);

process_asdu(#extend104_frame{payload = <<Type,VSQ:7,SQ:1,COT:16,Addr:16,Data/binary>>}) ->
	ASDU = #extend104_asdu{type=Type, vsq=VSQ, sq=SQ, cot=COT, addr=Addr, data=Data},
	process_asdu(Type, ASDU).

% 初始化结束
process_asdu(?M_EI_NA_1, _ASDU) ->
	?INFO_MSG("sub station reset!");

% 总召唤 激活确认
process_asdu(?M_IC_NA_1, #extend104_asdu{cot=?M_COT_ACTCON_1}) ->
	?INFO_MSG("recv M_IC_NA_1 M_COT_ACTCON_1");

% 总召唤 激活终止
process_asdu(?M_IC_NA_1, #extend104_asdu{cot=?M_COT_ACTTERM_1}) ->
	?INFO_MSG("recv M_IC_NA_1 M_COT_ACTTERM_1");
	%TODO:
	%m_pCtlInfo->bWaitData = FALSE;
	%m_pCtlInfo->byWaitDataCnt=0;
	%m_pChannel->SaveEnd(pASDU->wAddr, VAR_YC);
	%m_pChannel->SaveEnd(pASDU->wAddr, VAR_YX);


% 电能脉冲计数量帧
process_asdu(?M_IT_NA_1, #extend104_asdu{sq=0}) -> 
	%TODO: ProcessM_IT_NA(pBuf);
	%IT_CR_SIGN_MESSAGE* pITCR = (IT_CR_SIGN_MESSAGE*)pBuf;
	%for (int i=0; i<pAsduPub->byVSQ; i++)
	%{
	%	if (!pITCR->bIV)
	%		m_pChannel->SaveDD(pAsduPub->wAddr, pITCR->GetAddr()/*-MIN_IT_ADDR*/, pITCR->nCount);
	%	pITCR++;
	%}
	ok;

process_asdu(?M_IT_NA_1, #extend104_asdu{sq=1}) -> 
	%TODO:
	%MESSAGE_SIGN* pSign = (MESSAGE_SIGN*)pBuf;
	%pBuf+=sizeof(MESSAGE_SIGN);
	%USHORT usOrder = pSign->GetAddr()/*-MIN_IT_ADDR*/;
	%IT_CR* pCR = (IT_CR*)pBuf;
	%for (int i=0; i<pAsduPub->byVSQ; i++)
	%{
	%	if (!pCR->bIV)
	%		m_pChannel->SaveDD(pAsduPub->wAddr, usOrder, pCR->nCount);
	%	usOrder++;
	%	pCR++;
	%}
	ok;

% 电度命令确认
process_asdu(?M_CI_NA_1, #extend104_asdu{cot=COT}) ->
	if
	COT == ?M_COT_ACTCON_1 -> %电度脉冲召唤确认命令
		?INFO_MSG("recv M_CI_NA start.");
	COT == ?M_COT_ACTTERM_1 -> %电度脉冲传送完毕
		%m_pCtlInfo->bWaitData = FALSE;
		%m_pCtlInfo->byWaitDataCnt = 0;
		?INFO_MSG("recv M_CI_NA over.");
	true ->
		?ERROR("bad M_CI_NA COT: ~p", [COT])
	end;

% 单点NA
process_asdu(?M_SP_NA_1, #extend104_asdu{sq=0}) ->
	?INFO_MSG("recv M_SP_NA_1 frame");
	%TODO: ProcessM_SP_NA(pBuf);
	%SP_NA_SIGN_MESSAGE* pSPNA = (SP_NA_SIGN_MESSAGE*)pBuf;
	%for (int i=0; i<pAsduPub->byVSQ; i++)
	%{
	%	if (!pSPNA->bIV)
	%	{
	%		m_pChannel->SaveYX(pAsduPub->wAddr, pSPNA->GetAddr() /*- MIN_SP_ADDR*/, pSPNA->bSPI);
	%	}
	%	pSPNA++;
	%}
	%m_pChannel->SaveEnd(pAsduPub->wAddr, VAR_YX);

process_asdu(?M_SP_NA_1, #extend104_asdu{sq=1}) ->
	%MESSAGE_SIGN* pSign = (MESSAGE_SIGN*)pBuf;
	%pBuf += sizeof(MESSAGE_SIGN);
	%USHORT usOrder = pSign->GetAddr()/*-MIN_SP_ADDR*/;
	%SP_NA* pSPNA = (SP_NA*)pBuf;
	%for (int i=0; i<pAsduPub->byVSQ; i++)
	%{
	%	if (!pSPNA->bIV)
	%		m_pChannel->SaveYX(pAsduPub->wAddr, usOrder, pSPNA->bSPI);
	%	usOrder++;
	%	pSPNA++;
	%}
	ok;

% CP56Time2a，区别M_SP_TA_1 CP24Time2a
process_asdu(?M_SP_TB_1, ASDU) ->
	%TODO: ProcessM_SP_TB(pBuf);
	%pBuf += sizeof(ASDU_PUB);
	%BYTE byInfoNum = pAsduPub->byVSQ & 0x7f;
    %
	%CCSSOEPUB soeData;
	%memset(&soeData, 0, sizeof(CCSSOEPUB));
    %
	%VARDATA_TYPE eType = GENSOE;
	%for (int i=0; i<byInfoNum; i++)
	%{
	%	SP_TB_MESSAGE* pSPTB = (SP_TB_MESSAGE*)pBuf;
	%	if (pSPTB->GetAddr() >= MIN_SP_ADDR && pSPTB->GetAddr() <= MAX_SP_ADDR)//遥信变位
	%	{
	%		eType = GENSOE;	
	%		soeData.wYXPoint = pSPTB->GetAddr()-MIN_SP_ADDR;
	%	}
	%	else if (pSPTB->GetAddr() >= MIN_BH_ADDR && pSPTB->GetAddr() <= MAX_BH_ADDR) //保护事件
	%	{
	%		eType = BHSOE;
	%		soeData.wYXPoint = pSPTB->GetAddr()-MIN_BH_ADDR;
	%		soeData.byAddr = (BYTE)pAsduPub->wAddr;
	%	}
	%	else
	%		return;
    %
	%	soeData.byYXState = pSPTB->bSPI;
    %
	%	SYSTEMTIME sysTime;
	%	GetLocalTime(&sysTime);
	%	soeData.wMSecond	= pSPTB->tm.wMilSecond %1000;		// 毫秒
	%	soeData.bySecond	= pSPTB->tm.wMilSecond /1000;		// 秒
	%	soeData.byMinute	= pSPTB->tm.byMinute;				// 分
	%	soeData.byHour		= pSPTB->tm.byHour;				// 时(使用当前系统时间)
	%	soeData.byDay		= pSPTB->tm.byDay;				// 日(使用当前系统时间)
	%	soeData.byMonth		= pSPTB->tm.byMonth;				// 月(使用当前系统时间)
    %
	%	m_pChannel->SaveSOEALL(pAsduPub->wAddr, eType, &soeData);			//	SOE入队
    %
	%	pBuf += sizeof(SP_TB_MESSAGE);
	%}
    %
	%m_pChannel->SaveEnd(pAsduPub->wAddr, eType);
	ok;

% 双点NA
process_asdu(?M_DP_NA_1, #extend104_asdu{sq=0}) ->
	?INFO_MSG("recv M_DP_NA_3 frame"),
	%TODO: ProcessM_DP_NA(pBuf);
	%	DP_NA_SIGN_MESSAGE* pDPNA = (DP_NA_SIGN_MESSAGE*)pBuf;
	%	for (int i=0; i<pAsduPub->byVSQ; i++)
	%	{
	%		if (pDPNA->GetAddr()<MIN_SP_ADDR || pDPNA->GetAddr()>MAX_SP_ADDR)
	%		{
	%			pDPNA++;
	%			continue;
	%		}
	%		if (pDPNA->bDPI==1 || pDPNA->bDPI==2)
	%		{
	%			if (!pDPNA->bIV)
	%			{
	%				m_pChannel->SaveYX(pAsduPub->wAddr, pDPNA->GetAddr()-MIN_SP_ADDR, pDPNA->bDPI==1 ? FALSE:TRUE);
	%			}
	%		}
	%			
	%		pDPNA++;
    %
	%	}
	ok;

process_asdu(?M_DP_NA_1, #extend104_asdu{sq=1}) ->
	%MESSAGE_SIGN* pSign = (MESSAGE_SIGN*)pBuf;
	%if (pSign->GetAddr()<MIN_SP_ADDR || pSign->GetAddr()>MAX_SP_ADDR)
	%	return;
    %
	%pBuf+=sizeof(MESSAGE_SIGN);
	%USHORT usOrder = pSign->GetAddr()-MIN_SP_ADDR;
	%DP_NA* pDPNA = (DP_NA*)pBuf;
    %
	%for (int i=0; i<pAsduPub->byVSQ; i++)
	%{
	%	if (pDPNA->bDPI==1 || pDPNA->bDPI==2)
	%	{
	%		if (!pDPNA->bIV)
	%		{
	%			m_pChannel->SaveYX(pAsduPub->wAddr, usOrder, pDPNA->bDPI==1 ? FALSE:TRUE);
	%		}
	%	}
	%		
	%	usOrder++;
	%	pDPNA++;
	%}
	ok;

% 成组单点信息
process_asdu(?M_PS_NA_1, #extend104_asdu{sq=0}) ->
	%TODO: ProcessM_PS_NA(pBuf);
	
	%MESSAGE_SIGN* pSign = (MESSAGE_SIGN*)pBuf;
	%pBuf += sizeof(MESSAGE_SIGN);
	%if (pSign->GetAddr()<MIN_SP_ADDR || pSign->GetAddr()+16*pAsduPub->byVSQ > MAX_SP_ADDR)
	%{
	%	return;
	%}
	%PS_NA* pPSNA = (PS_NA*)pBuf;
	%for (int i=0; i<pAsduPub->byVSQ; i++)
	%{
	%	WORD wVal = pPSNA->wBS;
	%	USHORT usIndex = pSign->GetAddr() - MIN_SP_ADDR + i*16;
	%	for (int j=0;j<16;j++)
	%	{
	%		BOOL bYxVal = wVal & 0x01;
	%		wVal >>=1;
	%		m_pChannel->SaveYX(pAsduPub->wAddr, usIndex+j, bYxVal);
	%	}
    %
	%	pPSNA++;
	%}
    %
	%m_pChannel->SaveEnd(pAsduPub->wAddr, VAR_YX);
	ok;

process_asdu(?M_PS_NA_1, #extend104_asdu{sq=1}) ->
	ignore;

% 测量值--归一化值
process_asdu(?M_ME_NA_1, ASDU) ->
	process_M_ME_NA(ASDU);
% 测量值--标度化值，与归一化相同处理
process_asdu(?M_ME_NB_1, ASDU) ->
	process_M_ME_NA(ASDU);

%:// 双点遥控命令(镜像)
process_asdu(?M_DC_NA_1, ASDU) ->
	%BYTE byFjStatus = FX_CCS_YK_RIGHT;
	%m_pCtlInfo->bWaitData = 0;
	%if (memcmp(pStart +4, m_pCtlInfo->pCommandBuf+1+4, m_pCtlInfo->pCommandBuf[0]-4)!=0)	// 镜像帧 +4比较传送原因以后的数据，因为101传送原因不同
	%	byFjStatus = FX_CCS_YK_ERROR;
	%ASDU_PUB* pAsdupub = (ASDU_PUB*)pStart;
	%DC_NA_MESSAGE* pDCNA = (DC_NA_MESSAGE*)(pStart+sizeof(ASDU_PUB));
	%if (pAsdupub->wCOT == M_COT_ACTCON_1)
	%{
	%	if (pDCNA->bSE)//选择
	%	{
	%		m_pChannel->SaveYKFX(pAsdupub->wAddr, byFjStatus);
	%		m_pChannel->SaveEnd(pAsdupub->wAddr, YKFJ);
	%		TRACE0("recv ykfx frame\n");
	%		return;
	%	}
	%	if (!pDCNA->bSE) //执行
	%	{
	%		TRACE0("recv yk ykexe frame\n");
	%		return;
	%	}
	%}
	%else if (pAsdupub->wCOT == M_COT_DEACTCON_1)
	%{
	%	TRACE0("recv cancel  frame\n");
	%	return;
	%}
	%else if(pAsdupub->wCOT == 0x47)// 失败,返回COT=0x47
	%{
	%	byFjStatus = FX_CCS_YK_ERROR;
	%	m_pChannel->SaveYKFX(pAsdupub->wAddr, byFjStatus);
	%	m_pChannel->SaveEnd(pAsdupub->wAddr, YKFJ);
	%}
	%m_pChannel->ResetCommand();
	
	ok;
    
process_asdu(?M_EXT_LEAD, ASDU) ->
	%ProcessM_EXT_LEAD(pBuf);
	ok;

process_asdu(?C_SE_NA_1Ex, ASDU) ->
	%TODO: ProcessC_SE_NA_1Ex(pBuf);
	%BYTE byFjStatus = FX_CCS_YK_RIGHT;
	%m_pCtlInfo->bWaitData = 0;
    %
	%if (memcmp(pStart +4, m_pCtlInfo->pCommandBuf+1+4, m_pCtlInfo->pCommandBuf[0]-4)!=0)	// 镜像帧 +4比较传送原因以后的数据，因为101传送原因不同
	%	byFjStatus = FX_CCS_YK_ERROR;
    %
	%ASDU_PUB* pAsdupub = (ASDU_PUB*)pStart;
    %
	%SE_NA_MESSAGE* pSENA = (SE_NA_MESSAGE*)(pStart+sizeof(ASDU_PUB));
	%if (pAsdupub->wCOT == M_COT_ACTCON_1)
	%{
	%	if (pSENA->bSE)//选择
	%	{
	%		m_pChannel->SaveYKFX(pAsdupub->wAddr, byFjStatus,false);
	%		m_pChannel->SaveEnd(pAsdupub->wAddr, YKFJ);
	%		TRACE0("recv ykfx frame\n");
	%		return;
	%	}
	%	if (!pSENA->bSE) //执行
	%	{
	%		TRACE0("recv yk ykexe frame\n");
	%		return;
	%	}
	%}
	%else if (pAsdupub->wCOT == M_COT_DEACTCON_1)
	%{
	%	TRACE0("recv cancel  frame\n");
	%	return;
	%}
	%else if(pAsdupub->wCOT == 0x47)// 失败,返回COT=0x47
	%{
	%	byFjStatus = FX_CCS_YK_ERROR;
	%	m_pChannel->SaveYKFX(pAsdupub->wAddr, byFjStatus);
	%	m_pChannel->SaveEnd(pAsdupub->wAddr, YKFJ);
	%}
	%m_pChannel->ResetCommand();
	ok;

process_asdu(?TYPE_CHARGE, ASDU) ->
	%//跨越ASDU公共区域
	%pBuf += 6;
	%//跨越信息对象地址
	%pBuf += 3;
	%m_pChannel->AddPubBuf(STR_CHARGE,strlen(STR_CHARGE),pBuf,nLen-15);
	ok;

process_asdu(?TYPE_SWAP, ASDU) ->
	%//跨越ASDU公共区域
	%pBuf += 6;
	%//跨越信息对象地址
	%pBuf += 3;
	%m_pChannel->AddPubBuf(STR_SWAP,strlen(STR_SWAP),pBuf,nLen-15);
	ok;

process_asdu(Type, Payload) ->
	?ERROR("Unexepected ASDU: {~p, ~p}", [Type, Payload]).

process_M_ME_NA(ADSU=#extend104_asdu{sq=0}) ->
	%MEA_NA_SIGN_MESSAGE* pMENA = (MEA_NA_SIGN_MESSAGE*)pBuf;
	%for (int i=0; i<pAsduPub->byVSQ; i++)
	%{
	%	int n = pMENA->GetAddr()/*-MIN_YC_ADDR*/;
	%	if(!pMENA->bIV)
	%	{
	%		m_pChannel->SaveYC(pAsduPub->wAddr, n, pMENA->sMVAL);
	%	}			
	%	pMENA++;
	%}
	ok;

process_M_ME_NA(ADSU=#extend104_asdu{sq=1}) ->
	%MESSAGE_SIGN* pSign = (MESSAGE_SIGN*)pBuf;
	%pBuf+=sizeof(MESSAGE_SIGN);
	%USHORT usOrder = pSign->GetAddr()/*-MIN_YC_ADDR*/;
	%MEA_NA* pMENA = (MEA_NA*)pBuf;
    %
	%for (int i=0; i<pAsduPub->byVSQ; i++)
	%{
	%	if(!pMENA->bIV)
	%	{
	%		m_pChannel->SaveYC(pAsduPub->wAddr, usOrder, pMENA->sMVAL);
	%	}
	%	
	%	usOrder++;
	%	pMENA++;
	%}
	ok.



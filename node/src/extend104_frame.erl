%%%----------------------------------------------------------------------
%%% File    : extend104_frame.erl
%%% Author  : Ery Lee <ery.lee@gmail.com>
%%% Purpose : Parse and serialise extend104 frame.
%%% Created : Oct. 10 2013
%%% License : http://opengoss.com
%%%
%%% Copyright opengoss.com 2013 
%%%----------------------------------------------------------------------
-module(extend104_frame).

-include("extend104.hrl").

-include("extend104_frame.hrl").

-include_lib("elog/include/elog.hrl").

-import(extend104_util, [reverse_byte_value/1, reverse_byte_value2/1]).

-export([parse/1,
		serialise/1,
		process_asdu/1]).
		

parse(<<C1, C2, C3, C4, Payload/binary>>) ->
	#extend104_frame{c1=C1, c2=C2, c3=C3, c4=C4, payload=Payload}.

serialise(Frame) when is_record(Frame, extend104_frame) ->
	#extend104_frame{c1=C1, c2=C2, c3=C3, c4=C4, payload=Payload} = Frame,
	Len = size(Payload) + 4,
	<<16#68, Len, C1, C2, C3, C4, Payload/binary>>;
serialise(Frame) when is_record(Frame, extend104_asdu) ->
	#extend104_asdu{type=Type, sq=SQ, vsq=VSQ, cot=COT, addr=Addr, data=Data} = Frame,	
	<<Type, SQ:1,VSQ:7,COT:8, 0, Addr/binary, Data/binary>>.
							
% start_cont | stop_cont
process_asdu(Frame = #extend104_frame{payload = <<>>}) ->
	% TODO
	?ERROR("empty ~p", [Frame]);
process_asdu(#extend104_frame{payload = <<Type,SQ:1,VSQ:7,COT:8,_COT:1/binary,Addr:2/binary,Data/binary>>}) ->
	ASDU = #extend104_asdu{type=Type, sq=SQ, vsq=VSQ, cot=COT, addr=extend104_util:reverse_byte(Addr), data=Data},
	case process_asdu(Type, ASDU) of
		ok -> ok;
		{response, CommandData} ->
			{response, CommandData};
		{status, S} ->
			{response, {status, S} };
		{data, DataF} ->
			%TODO pare business data
			?ERROR("get data:~p,~p", [Type, DataF]);
		{datalist, DataList} ->
			?INFO("get asdu:~p,data length:~p, ~n ~p", [ASDU#extend104_asdu{data= <<>>}, length(DataList) == VSQ, DataList]),
			DataList1 = lists:map(fun({PAddr, Value}) ->
				#measure{address=reverse_byte_value(Addr), type=Type, no=reverse_byte_value(PAddr), cot=COT, value = Value}
			end, DataList),
			{measure, DataList1}
	end.
	
%%----------------- 交互返回 			
	
% 100:总召确认 7	
process_asdu(100, #extend104_asdu{cot= ?M_COT_ACTIVE_1} = ASDU) ->
	?INFO("all confirm :~p",[ASDU]);
% 100:总召结束 10
process_asdu(100, #extend104_asdu{cot= ?M_COT_ACTTERM_1} = ASDU) ->
	?INFO("all over :~p",[ASDU]),
	{status, over};
% 101:计算量总召确认 7	
process_asdu(101, #extend104_asdu{cot= ?M_COT_ACTIVE_1} = ASDU) ->
	?INFO("all count confirm :~p",[ASDU]);
% 101:计算量总召结束 10	
process_asdu(101, #extend104_asdu{cot= ?M_COT_ACTTERM_1} = ASDU) ->
	?INFO("all count over:~p",[ASDU]),
	{status, over};

% 103:时钟同步确认 7
process_asdu(103, #extend104_asdu{cot= ?M_COT_ACTIVE_1} = ASDU) ->
	?INFO("all time confirm :~p",[ASDU]),
	{status, confirm};	
	
% 46:双点遥控 7
process_asdu(46, #extend104_asdu{cot=Cot, data = Data} = ASDU) ->
	?INFO("yk_46 :~p",[ASDU]),
	{response, {data, Cot, Data}};
				
% 52:设点命令 7
process_asdu(52, #extend104_asdu{cot=Cot, data = Data} = ASDU) ->
	?INFO("sd_52 :~p",[ASDU]),
	{response, {data, Cot, Data}};			
	
% 55:调度命令 7
process_asdu(55, #extend104_asdu{cot=Cot, data = Data} = ASDU) ->
	?INFO("dd_55 :~p",[ASDU]),					
	{response, {data, Cot, Data}};


%%----------------- 非交互返回 
% 1:单点NA
process_asdu(?M_SP_NA_1, ASDU) ->
	process_M_SP_NA(ASDU);
	
% 11:测量值--标度化值，与归一化相同处理
process_asdu(?M_ME_NB_1, ASDU) ->
	process_M_ME_NA(ASDU);
	
% 15:电能脉冲计数量帧
process_asdu(?M_IT_NA_1, ASDU) -> 
	process_M_IT_NA(ASDU);
	
process_asdu(130, #extend104_asdu{sq=0, data= <<RecordType, Rest>>}) -> 
	process_130(RecordType, Rest);
	
process_asdu(131, #extend104_asdu{sq=0, data= <<No:17/binary, FaultType:16, ActionType, CurrTime:7/binary>>}) -> 
	?INFO("get info:~p",[{No, FaultType, ActionType, CurrTime}]),
	{data, []};		
	
process_asdu(132, #extend104_asdu{sq=0, data= <<DY:16,SOC:16,WD:16,CountWD:16,CountDC:16,Rest>>}) -> 
	{DataWD, Rest2} = process_132(CountWD, Rest, []),
	{DataDY, _} = process_132(CountWD, Rest, []),
	{data, []};
	
process_asdu(133, #extend104_asdu{sq=0, data= <<Status:16, Fault>>}) -> 
	{data, []};				
	
process_asdu(134, #extend104_asdu{sq=0, data= <<ZDL:32, FDL:32>>}) -> 
	{data, []};					
	
process_asdu(135, #extend104_asdu{sq=0, data= <<No:17/binary, Rest>>}) -> 
	process_135(Rest, []);
	
process_asdu(136, #extend104_asdu{sq=0, data= <<RecordType, Rest>>}) -> 
	process_136(RecordType, Rest);			
						
process_asdu(Type, Payload) ->
	?ERROR("Unexepected ASDU: {~p, ~p}", [Type, Payload]).





% 1：M_SP_NA_1 遥信
process_M_SP_NA(#extend104_asdu{sq=0, data=Data}) ->
	process_M_SP_NA(0, Data, []);
process_M_SP_NA(#extend104_asdu{sq=1, data = <<PAddr:3/binary,Other/binary>>}) ->
	[{PAddr, process_M_SP_NA(1, Other, [])}].

% content
process_M_SP_NA(_SQ, <<>>, Acc) ->
	{datalist, Acc};
process_M_SP_NA(0, <<PAddr:3/binary, IV:1, NT:1, SB:1,BL:1,SPI:4,Other/binary>>, Acc) ->
	% ?INFO("get addr:~p,info :~p", [RPAddr, {IV,NT,SB,BL,SPI}]),
	NewAcc = check_iv(IV, {PAddr, SPI}, Acc),
	process_M_SP_NA(0, Other, NewAcc);
process_M_SP_NA(1, <<IV:1, NT:1, SB:1,BL:1,SPI:4,Other/binary>>, Acc) ->
	% ?INFO("get info :~p", [{IV,NT,SB,BL,SPI}]),
	NewAcc = check_iv(IV, SPI, Acc),
	process_M_SP_NA(1, Other, NewAcc).


% 11：M_ME_NA_1 遥测
process_M_ME_NA(#extend104_asdu{sq=0, data=Data}) ->
	process_M_ME_NA(0, Data, []);
process_M_ME_NA(#extend104_asdu{sq=1, data = <<PAddr:3/binary,Other/binary>>}) ->
	[{PAddr, process_M_ME_NA(1, Other, [])}].
	
process_M_ME_NA(_SQ, <<>>, Acc) ->
	{datalist, Acc};
process_M_ME_NA(0, <<PAddr:3/binary, Value:2/binary, IV:1, NT:1, SB:1,BL:1,OV:4,Other/binary>>, Acc) ->
	NewAcc = check_iv(IV, {PAddr, reverse_byte_value2(Value)}, Acc),
	process_M_ME_NA(0, Other, NewAcc);
process_M_ME_NA(1, <<Value:2/binary, IV:1, NT:1, SB:1,BL:1,OV:4,Other/binary>>, Acc) ->
	NewAcc = check_iv(IV, reverse_byte_value2(Value), Acc),
	process_M_ME_NA(1, Other, NewAcc).

% 15：M_IT_NA_1	计数量
process_M_IT_NA(#extend104_asdu{sq=0, data=Data}) ->
	process_M_IT_NA(0, Data, []);
process_M_IT_NA(#extend104_asdu{sq=1, data = <<PAddr:3/binary,Other/binary>>}) ->
	[{PAddr, process_M_IT_NA(1, Other, [])}].
	
process_M_IT_NA(_, <<>>, Acc) ->
	{datalist, Acc};
process_M_IT_NA(0, <<PAddr:3/binary, Value:4/binary, IV:1, CA:1, CY:1,SQ:5,Other/binary>>, Acc) ->
	NewAcc = check_iv(IV, {PAddr, reverse_byte_value(Value)}, Acc),
	process_M_IT_NA(0, Other, NewAcc);
process_M_IT_NA(1, <<Value:4/binary, IV:1, CA:1, CY:1,SQ:5,Other/binary>>, Acc) ->
	NewAcc = check_iv(IV, reverse_byte_value(Value), Acc),
	process_M_IT_NA(1, Other, NewAcc).	
	

check_iv(0, Value, Acc) ->
	[Value|Acc];
check_iv(1, Value, Acc) ->
	% ?ERROR("invaild vaule:~p", [Value]),
	Acc.	


% 130	
process_130(1, <<StakeType,SerialNo:19/binary,StakeNo:17/binary,CardNo:4/binary, Begin:7/binary,CDType,
				DCType, DY:16,DL:16,Mode, ModeArgs:4/binary, YK, YKJE:32, ZDD:32>>) ->
	{data, []};
process_130(2, <<StakeType,SerialNo:19/binary,StakeNo:17/binary,CardNo:4/binary, Begin:7/binary,End:7/binary,CDType,
				DCType, DY:16,DL:16,Mode,ModeArgs:4/binary, Reason, JDJ:32,JDL:32,JJE:32,FDJ:32,FDL:32,FJE:32,
				PDJ:32,PDL:32,PJE:32,GDJ:32,GDL:32,ZDL:32,ZDD:32>>) ->
	{data, []};
process_130(3, <<StakeType,SerialNo:19/binary,StakeNo:17/binary,CardNo:4/binary, Begin:7/binary,End:7/binary,
				Checkout:7/binary, CostDL:32, CostJE:32>>) ->
	{data, []}.				

process_132(0, Rest, Acc) ->
	{Acc, lists:reverse(Rest)};		
process_132(Count, <<Data:16,Rest>>, Acc) ->
	process_132(Count - 1, Rest, [Data|Acc]).		
	
process_135(<<GetTime:4/binary>>, Acc) ->
	{data, lists:reverse(Acc)};	
process_135(<<CellNo:2/binary,DcxNo:18/binary,CdjNo:17/binary,Rest>>, Acc) ->
	process_135(Rest,[{CellNo, DcxNo, CdjNo}|Acc]).
	
process_136(1, <<CardNO:16/binary,PW:8/binary,TM:4/binary>>) ->
	{data, []};
process_136(2, <<CardNO:16/binary,PW:8/binary,StakeNo:17/binary,DBZ:4/binary,DBF:4/binary,DBG:4/binary,
				DBP:4/binary,TM:4/binary>>) ->
	{data, []};	
process_136(3, <<CardNO:16/binary,PW:8/binary,StakeNo:17/binary,DBZ:4/binary,DBF:4/binary,DBG:4/binary,
				DBP:4/binary,TM:4/binary>>) ->
	{data, []};		
process_136(4, _) ->
	{data, []}.	




	
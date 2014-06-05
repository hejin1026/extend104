
-record(extend104_frame, {
		c1 = 0		:: integer(),
		c2 = 0		:: integer(),
		c3 = 0		:: integer(),
		c4 = 0 		:: integer(),
		payload	= <<>> :: binary()}).

-record(extend104_asdu, {
		type, 
		sq,
		vsq,
		cot,
		addr,
		data}).


-define(FRAME_STARTDT, #extend104_frame{c1=2#0111}).

-define(FRAME_STARTDT_END, #extend104_frame{c1=2#1011}).

-define(FRAME_STOPDT, #extend104_frame{c1=2#010011}).

-define(FRAME_CONFIRM, #extend104_frame{c1=1,c2=0}).

-define(FRAME_TESTFR_SEND, #extend104_frame{c1=16#43}).
-define(FRAME_TESTFR_REPLY, #extend104_frame{c1=16#83}).

%<<68, 16#0E, 00, 00, 06, 00, 100, 01, 06, 00, 01, 00, 00, 00, 00, 14>>) | 6,1,1,2,2 + 3,1
-define(FRAME_100(Cid), #extend104_frame{payload = <<16#64, 01, 06, 00, Cid/binary, 00, 00, 00, 16#14>>}).

-define(FRAME_101(Cid), #extend104_frame{payload = <<16#65, 01, 06, 00, Cid/binary, 00, 00, 00, 16#14>>}).

-define(FRAME_103(Cid), #extend104_frame{payload = <<16#67, 01, 06, 00, Cid/binary, 00, 00, 00, 16#14>>}).

% 针对测点 Data:2#10000001
-define(FRAME_46(Cot, Taddr, Maddr, Data), #extend104_frame{payload = <<46, 01, Cot, 00, Taddr/binary, Maddr/binary, Data/binary>>}). 
-define(FRAME_52(Cot, Taddr, Maddr, Data, SE), #extend104_frame{payload = <<52, 01, Cot, 00, Taddr/binary, Maddr/binary, Data:32/float, SE:1,0:7>>}). 
-define(FRAME_55(Num, Cot, Taddr, SE, MI,OV,IN, BDS), #extend104_frame{payload = <<55, Num, Cot, 00, Taddr/binary, SE:1,MI:1,OV:1,IN:5, BDS/binary>>}). 



% ASDU Type
-define(C_IC_NA_1,		100).	% 总召唤命令
-define(C_CI_NA_1,		101).	% 电能脉冲计数量冻结命令/
-define(C_CS_NA_1,		103).	% 时间同步

-define(M_SP_NA_1,		1).	% 单点
-define(M_ME_NA_1,		9).	% 测量值--归一化值
-define(M_ME_NB_1,      11).% 测量值--标度化值
-define(M_IT_NA_1,		15).% 电能脉冲计数量帧


% COT
-define(M_COT_ACTIVE,		6). %去激活
-define(M_COT_STOP, 		8). %停止激活

-define(M_COT_ACTIVE_1,     7). %激活确认
-define(M_COT_STOP_1,		9). %停止激活确认		
-define(M_COT_ACTTERM_1,    10). %激活终止




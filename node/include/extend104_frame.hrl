
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

-define(FRAME_STOPDT, #extend104_frame{c1=2#010011}).

-define(FRAME_CONFIRM, #extend104_frame{c1=1,c2=0}).

-define(FRAME_TESTFR_SEND, #extend104_frame{c1=16#43}).
-define(FRAME_TESTFR_REPLY, #extend104_frame{c1=16#83}).

%<<68, 16#0E, 00, 00, 06, 00, 64, 01, 06, 00, 01, 00, 00, 00, 00, 14>>),
-define(FRAME_C_IC_NA_1(Cid), #extend104_frame{payload = <<16#64, 01, 06, 00, Cid/binary, 00, 00, 00, 16#14>>}).

-define(FRAME_C_CI_NA_1(Cid), #extend104_frame{payload = <<16#65, 01, 06, 00, Cid/binary, 00, 00, 00, 16#14>>}).

-define(FRAME_C_CS_NA_1(Cid), #extend104_frame{payload = <<16#67, 01, 06, 00, Cid/binary, 00, 00, 00, 16#14>>}).



% ASDU Type
-define(C_CS_NA_1,		103).	% 时间同步
-define(C_IC_NA_1,		100).	% 总召唤命令
-define(C_CI_NA_1,		101).	% 电能脉冲计数量冻结命令/

-define(M_SP_NA_1,		1).	% 单点
-define(M_ME_NA_1,		9).	% 测量值--归一化值
-define(M_ME_NB_1,      11).% 测量值--标度化值
-define(M_IT_NA_1,		15).% 电能脉冲计数量帧
-define(M_EI_NA_1,      70).    % 初始化结束


% COT
-define(M_COT_ACTCON_1,     16#7). %激活确认
-define(M_COT_ACTTERM_1,    16#0A). %激活终止





-define(APCI_I, 16#00).
-define(APCI_S, 16#01).
-define(APCI_U, 16#03).

%function code
-define(FUN_CODE_GLB,	255).		% 全局功能类型
-define(FUN_CODE_GEN,	254).		% 通用分类功能
-define(FUN_CODE_DISTP,	128).		% 距离保护

% address 
-define(PUBLIC_ADDRESS,	100).

% startup symbol
-define(STARTUP_SYMBOL,	16#68).

% ST value offset
-define(OFFSET_ST_VALUE, 64).

% U-APCI the first byte value
-define(APCI_TESTFR_CONFIRM, 	16#83).
-define(APCI_TESTFR_EFFECT,		16#43).
-define(APCI_STOPDT_CONFIRM, 	16#23).
-define(APCI_STOPDT_EFFECT,		16#13).
-define(APCI_STARDT_CONFIRM, 	16#0b).
-define(APCI_STARDT_EFFECT,		16#07).

% ASDU Type
-define(C_CS_NA_1,		103).	% 时间同步
-define(C_IC_NA_1,		100).	% 总召唤命令
-define(M_IC_NA_1,		100).	% 总召唤命令
-define(C_EXT_LEAD,		137).	% 力导扩展用命令字，例如保护定值
-define(M_EXT_LEAD,     137). % 同上，上送。

-define(M_IT_NA_1,		15).% 电能脉冲计数量帧
-define(M_ST_NA_1,		5).	% 步位置--
-define(M_ST_TA_1,		6).	% 步位置--带时标
-define(M_SP_NA_1,		1).	% 单点
-define(M_SP_TA_1,		2).	%	
-define(M_SP_TB_1,		30).%	
-define(M_DP_NA_1,		3).	% 双点
-define(M_DP_TA_1,		4).	%
-define(M_PS_NA_1,      20).% 成组单点信息
-define(M_ME_NA_1,		9).	% 测量值--归一化值
-define(M_ME_NB_1,      11).% 测量值--标度化值
-define(C_SC_NA_1,		45). % 单点遥控命令
-define(C_DC_NA_1,		46). % 双点遥控命令
-define(M_DC_NA_1,		46). % 双点遥控命令(镜像)
-define(C_RC_NA_1,		47).  % 步调节命令
-define(M_RC_NA_1,		47).  % 步调节命令(镜像)
-define(C_SE_NA_1Ex,	52).	% 设点命令，扩展自C_SE_NA_1=48
-define(C_CI_NA_1,		101).	% 电能脉冲计数量冻结命令/
-define(M_CI_NA_1,		101).	% 电能脉冲计数量冻结命令确认
-define(M_EP_TA_1,      17).   % 带时标的继电保护设备事件                         
-define(M_EP_TB_1,      18).   % 带时标的继电保护设备成组启动事件                  
-define(M_EP_TC_1,      19).   % 带时标的继电保护设备成组输出电路信息  
-define(M_EI_NA_1,      70).    % 初始化结束
-define(C_DEV_MEA,      16#4D).  % 申请测量值
-define(M_DEV_MEA,      16#4D).   % 装置测量值上送

-define(TYPE_CHARGE,	130).	%充电数据
-define(TYPE_SWAP,		133).	%换电数据

% COT
-define(C_COT_REQ_1,  		16#05). %请求/被请求
-define(M_COT_INIT_1,    	16#4). %初始化
-define(M_COT_SPO_1,		16#3). %突发
-define(M_COT_CYC_1,		16#1). %同步
-define(M_COT_REQ_1,     	16#5). %请求或者被请求
-define(M_COT_ACT_1,     	16#6). %激活
-define(M_COT_ACTCON_1,     16#7). %激活确认
-define(M_COT_DEACT_1,     	16#8). %停止激活
-define(M_COT_DEACTCON_1,   16#9). %停止激活确认
-define(M_COT_ACTTERM_1,    16#0A). %激活终止
-define(M_COT_INTROGEN_1, 	16#14). %响应站总召
-define(M_COT_REQCOGEN_1,  	16#25). %响应计数量总召

-define(DT_TOO_SEL,		1). %扰动数据TOO=1，召唤扰动数据表
-define(DT_TOO_CALL,    2). %扰动数据TOO=2，召唤数据

-define(MIN_SP_ADDR,	16#01).		%	单点遥信的信息体起始地址	
-define(MAX_SP_ADDR,	16#1000).		%	单点遥信的信息体终止地址	
%-define SP_RADIX		16#5a

-define(MIN_BH_ADDR,    16#1001).  %继电保护
-define(MAX_BH_ADDR,    16#4000).  %

-define(MIN_YC_ADDR,	16#4001).		%	遥测的信息体起始地址		
-define(MAX_YC_ADDR,	16#5000).		%	遥测的信息体终止地址		
%-define YC_RADIX		16#39

-define(MIN_IT_ADDR,	16#6401).		%	电度量的信息体起始地址		
-define(MAX_IT_ADDR,	16#6600).		%	电度量的信息体终止地址		
%-define IT_RADIX		16#1a

-define(MIN_YK_ADDR,	16#6001).		%	遥控量的信息体起始地址		
-define(MAX_YK_ADDR,	16#6200).		%	遥控量的信息体终止地址		
%-define YK_RADIX		16#1c

-define(MIN_ST_ADDR,	16#6601).		%	步位置偏移最小				
-define(MAX_ST_ADDR,	16#6700).		%	步位置偏移最大				
%-define ST_RADIX		16#4


% 通信过程控制定义相关定义
-define(MAX_VS_ACK_DIF,		12).	% 
-define(MAX_ALLOW_DELAY,	8).	%
-define(CONNECT_TIMEOUT, 	30).	%

-define(WAITACK_TIMEOUT,	15).	% wait ack
-define(SENDACK_TIMEOUT, 	10).	% send ack
-define(IDLE_TIMEOUT,		20).	% send test

% 定时计数
-define(MAX_IGC_TIME,		420).% 总查询  660
-define(MAX_CI_TIME,		230).% 电能脉冲

% 录波参数定义
-define(DTV_MAX_DATANUM, 	420).  %录波数据点数
-define(DTV_FRMAE_LEN_LIMIT,496). %录波数据帧长限制

% U格式控制域定义
-define(STARTDT_ACT, 	1).
-define(STARTDT_CON, 	2).
-define(STOPDT_ACT,		4).
-define(STOPDT_CON,		8).
-define(TESTFR_ACT,		16).
-define(TESTFR_CON,		32).



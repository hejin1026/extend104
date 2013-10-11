%TODO:
%-pragma pack(push)	
%-pragma pack(1)

typedef struct _tagI_APCIDATA
{
	WORD	byStyle:1,
			wTxSeq:15;
	WORD	byStyle1:1,
			wRxSeq:15;
}I_APCIDATA;

typedef struct _tagS_APCIDATA
{
	WORD	byStyle:2,
			wTxSeq:14;%no use
	WORD	byStyle1:1,
			wRxSeq:15;
}S_APCIDATA;

typedef struct _tagU_APCIDATA
{
	BYTE	byInfo[4];%no use		
}U_APCIDATA;

% ASDU公共部分
typedef struct _tagASDU_PUB
{
	BYTE byType;
	BYTE byVSQ:7,
		 bSQ:1;
	WORD wCOT;
	WORD wAddr;
}*PASDU_PUB,ASDU_PUB;
% 信息体标志
typedef struct _tagMESSAGE_SIGN
{
	BYTE byAddr1;
	BYTE byAddr2;
	BYTE byAddr3;
	DWORD GetAddr()
	{
		DWORD addr = MAKEWORD(byAddr2,byAddr3);
		return ((addr<<8) + byAddr1);
	}
	void SetAddr(DWORD value)
	{
		WORD wAddr = LOWORD(value); 
		byAddr1 = LOBYTE(wAddr);
		byAddr2 = HIBYTE(wAddr);
		byAddr3 = (BYTE)(value>>16);
	}
}*PMESSAGE_SIGN,MESSAGE_SIGN;
%总召
typedef struct _tagIGI_ASDU : public ASDU_PUB
{
	BYTE byInfoAddr[3];
	BYTE bySCN;
}*PIGI_ASDU,IGI_ASDU,*PC_CI_NA_ASDU,C_CI_NA_ASDU;

typedef struct _tagG_DATASET
{
	BYTE byAddr;%定值代码
	BYTE byDevType;%装置类型
	WORD wParam;
}*PG_DATASET,G_DATASET;

 % 二进制计数器读数CR
typedef struct _tagIT_CR	
{
	int		nCount;	% 计数器读数
	BYTE	bySeq:5,% 顺序号
			bCY:1,	% 进位
			bCA:1,	% 计数器被调整
			bIV:1;	% 有效
}*PIT_CR,IT_CR;

typedef struct _tagIT_CR_SIGN_MESSAGE : public 	MESSAGE_SIGN
{
	int		nCount;	% 计数器读数
	BYTE	bySeq:5,% 顺序号
			bCY:1,	% 进位
			bCA:1,	% 计数器被调整
			bIV:1;	% 有效
}*PIT_CR_SIGN_MESSAGE,IT_CR_SIGN_MESSAGE;

typedef struct _tagCP24Time2a
{
	WORD	wMilSecond;		%毫秒
	BYTE	byMinute:6,		%分
			byRes1:1,		%保留1
			bInvalid:1;		%无效
}*PCP24Time2a, CP24Time2a;
%CP32Time2a 四个八位位组的二进制时标
typedef struct _tagCP32Time2a
{
	WORD	wMilSecond;		%毫秒
	BYTE	byMinute:6,		%分
			byRes1:1,		%保留1
			bInvalid:1;		%无效
	BYTE	byHour:5,		%小时
			byRes2:2,		%保留2
			bSU:1;			%夏时制
}*PCP32Time2a,CP32Time2a;
% CP56Time2a 7个八位位组的二进制时标
typedef struct _tagCP56Time2a
{
	WORD	wMilSecond;		%毫秒
	BYTE	byMinute:6,		%分
			byRes1:1,		%保留1
			bInvalid:1;		%无效
	BYTE	byHour:5,		%小时
			byRes2:2,		%保留2
			bSU:1;			%夏时制
	BYTE	byDay:5,		%日
			byDayOfWeek:3;	%星期的某天
	BYTE	byMonth:4,		%月
			byRes3:4;		%保留3
	BYTE	byYear:4,		%年－2000
			byRes4:4;		%保留4
}*PCP56Time2a,CP56Time2a;
typedef struct _tagBCDParse
{
	WORD GetBCDVal(WORD wVal)
	{
		BYTE byVal[4];
		for (int i=0;i<4;i++)
		{
			byVal[i] = wVal & 16#0f;
			wVal >>= 4;
		}
		WORD wRel = 0;
		for (int i=3;i>=0;i--)
		{
			wRel = wRel*10 + byVal[i];
		}

		return wRel;
	}
}BCDPARSE;
%录波中7字节时间BCD码
typedef struct _tagTimeBCD56 : public BCDPARSE
{
	BYTE  byMonth;
	BYTE  byDay;
	BYTE  byHour;
	BYTE  byMinute;
	BYTE  bySecond;
	WORD  wMilSecond;

	BYTE GetMonth(){return (BYTE)GetBCDVal(byMonth);}
	BYTE GetDay(){return (BYTE)GetBCDVal(byDay);}
	BYTE GetHour(){return (BYTE)GetBCDVal(byHour);}
	BYTE GetMinute(){return (BYTE)GetBCDVal(byMinute);}
	BYTE GetSecond(){return (BYTE)GetBCDVal(bySecond);}
	WORD GetMilSecond(){return GetBCDVal(wMilSecond);}
}*PTimeBCD56, TimeBCD56;
% ST_TA 步位置带时标信息体
typedef struct _tagST_TA_MESSAGE :	public MESSAGE_SIGN
{
	BYTE	byValue:7,		% 值
			bTtransient:1;	% 瞬变
	BYTE	bOV:1,
			bRES:3,
			bBL:1,
			bSB:1,
			bNT:1,
			bIV:1;
	CP56Time2a tm;
}*PST_TA_MESSAGE,ST_TA_MESSAGE;


% SP_TA 单点带时标信息体
typedef struct _tagSP_TA_MESSAGE : public MESSAGE_SIGN
{
	BYTE	bSPI:1,
			bRes:3,
			bSB:1,
			bBL:1,
			bNT:1,
			bIV:1;
	CP24Time2a tm;
}*PSP_TA_MESSAGE,SP_TA_MESSAGE;
typedef struct _tagSP_TB_MESSAGE : public MESSAGE_SIGN
{
	BYTE	bSPI:1,
			bRes:3,
			bSB:1,
			bBL:1,
			bNT:1,
			bIV:1;
	CP56Time2a tm;
}*PSP_TB_MESSAGE,SP_TB_MESSAGE;
typedef struct _tagSP_NA_SIGN_MESSAGE : public MESSAGE_SIGN
{
	BYTE	bSPI:1,
			bRes:3,
			bSB:1,
			bBL:1,
			bNT:1,
			bIV:1;
}*PSP_NA_SIGN_MESSAGE,SP_NA_SIGN_MESSAGE;
typedef struct _tagSP_NA
{
	BYTE	bSPI:1,
			bRes:3,
			bSB:1,
			bBL:1,
			bNT:1,
			bIV:1;
}*PSP_NA,SP_NA;
typedef struct _tagPS_NA
{
	WORD wBS;
	WORD wCD;
	BYTE bOV:1,
		bRes:3,
		bBL:1,
		bSB:1,
		bNT:1,
		bIV:1;
}*PPS_NA, PS_NA;

% DP
typedef struct _tagDP_TA_MESSAGE : public MESSAGE_SIGN
{
	BYTE	bDPI:2,
			bRes:2,
			bSB:1,
			bBL:1,
			bNT:1,
			bIV:1;
	CP32Time2a tm;
}*PDP_TA_MESSAGE,DP_TA_MESSAGE;
typedef struct _tagDP_NA_SIGN_MESSAGE : public MESSAGE_SIGN
{
	BYTE	bDPI:2,
			bRes:2,
			bSB:1,
			bBL:1,
			bNT:1,
			bIV:1;
}*PDP_NA_SIGN_MESSAGE,DP_NA_SIGN_MESSAGE;
typedef struct _tagDP_NA
{
	BYTE	bDPI:2,
			bRes:2,
			bSB:1,
			bBL:1,
			bNT:1,
			bIV:1;
}*PDP_NA,DP_NA;

%MEA
typedef struct _tagMEA_NA
{
	short sMVAL;
	BYTE	bOV:1,
			bRes:3,
			bBL:1,
			bSB:1,
			bNT:1,
			bIV:1; 
}*PMEA_NA,MEA_NA;
typedef struct _tagMEA_NA_SIGN_MESSAGE : public MESSAGE_SIGN
{
	short sMVAL;
	BYTE	bOV:1,
			bRes:3,
			bBL:1,
			bSB:1,
			bNT:1,
			bIV:1; 
}*PMEA_NA_SIGN_MESSAGE,MEA_NA_SIGN_MESSAGE;

typedef struct _tagSE_NA_MESSAGE : public MESSAGE_SIGN
{
	float	dwYCVal;	%设定数值
	BYTE	bQOS:7,			
			bSE:1;  %0 ->exe 1->select
}*PSE_NA_MESSAGE,SE_NA_MESSAGE;

%DC
typedef struct _tagDC_NA_MESSAGE : public MESSAGE_SIGN
{
	BYTE	byDCS:2,
			byQU:5,
			%bACT:1,
			bSE:1;
}*PDC_NA_MESSAGE,DC_NA_MESSAGE,*PRC_NA_MESSAGE,RC_NA_MESSAGE;

% SYN
typedef struct _tagC_SYN_TA : public MESSAGE_SIGN
{
	CP56Time2a tm;
}*PSYN_TA_MESSAGE,SYN_TA_MESSAGE;

% REQ DZ
typedef struct _tagDZ_COMN
{
	BYTE byType;
	BYTE byInfoNum;
	BYTE byCOT;
	BYTE byAddr;
}*PDZ_COMN,DZ_COMN;

typedef struct _tagProtectionSOE : public BCDPARSE
{
	BYTE byAddr;   %装置地址
	BYTE byDay;     %日期
	BYTE byHour;    %小时
	BYTE byMinute;  %分钟
	BYTE bySecond;  %秒
	WORD wMilSecond; %毫秒
	BYTE byJumpNum;  %跳闸次数
	BYTE byErrType;  %故障类型
	BYTE byActionType; %动作类型
	BYTE byDtvType;  %装置类型
	BYTE byMeaType;   %测量类型
	WORD wMeaParam;  %测量参数
	BYTE GetDay(){return (BYTE)GetBCDVal(byDay);}
	BYTE GetHour(){return (BYTE)GetBCDVal(byHour);}
	BYTE GetMinute(){return (BYTE)GetBCDVal(byMinute);}
	BYTE GetSecond(){return (BYTE)GetBCDVal(bySecond);}
	WORD GetMilSecond(){return GetBCDVal(wMilSecond);}
}PROTSOE;
typedef struct _tagDTLIST_DATASET
{
	BYTE	byAddr_FAN; %录波编号
	BYTE	byFAN_FAN; %触发事件编号
	TimeBCD56  tmDtv[2];%启动结束时间    
}*PDTLIST_DATASET,DTLIST_DATASET;
% CALL DTV Configure
typedef struct _tagCallDTVConfg
{
	BYTE byRes1;
	BYTE byRes2;
	BYTE byOffset; %录波偏移数
}CALLDTVCONFG;
% call DTV data
typedef struct _tagCallDTVData
{
	BYTE byIndex; %录波编号
	BYTE byAddr; %现在为0
	WORD wStartPoint; %起始点号
	BYTE byPointNum; %召唤点数
}CALLDTVDATA;
% 扰动数据信息体尾
typedef struct _tagDT_DATA_TAIL
{
	WORD wFan;	%当前采样时刻的故障状态：<0>=无故障 ; <3>=故障
	WORD wTmSpan; %当前采样时刻P 的相对时标表示自保护启动时刻至本次采样时刻之间的相对时间T，
				%共16位，其时标值Tc = T*0.833- 100 ，单位毫秒(ms)。
}DT_DATA_TAIL;

%-----------------------------------------------------------
%录波配置表数据体定义
% 内存块组成－－开始
% DT_CONFG_HEAD
% DT_CONFG_HEAD.byChAna*DT_ANA_CH
% DT_CONFG_HEAD.byChDig*DT_DIG_CH
% DT_FREQ
% DT_HITS
% DT_HITS.wHitsNum*DT_HITS_ITEM
% DT_CONFG_TAIL
% 内存块组成－－结束

% 录波配置表尾
typedef struct _tagDT_CONFG_TAIL
{
	TimeBCD56 tmFirstPt;%第一个数据点时标
	TimeBCD56 tmTrigger;%录波触发点时标
}DT_CONFG_TAIL;
%_______________________________________________________________

typedef struct _tagDT_ValCtrl
{
	WORD wNDV;%本ASDU扰动值数目（NDV）
	WORD wNFE;%本ASDU第一个扰动值的序号（NFE）
}DT_VALCTRL;

typedef struct _tagDT_Value %using
{
	WORD wValue;
}DT_VALUE;

typedef struct _tagDTVCtrlInfo : public DT_CONFG_HEAD
{
	int dtvDevAddr; %召唤扰动数据的装置地址
	int dtvPtSpan; %扰动数据一次召唤点数
	int dtvPtStart; %扰动数据召唤起始点号
	int dtvPtMax; %扰动数据最大数据点数
	int dtvUnitAddr; %通讯总控地址
	int byIndex; %录波号
}DTVCTRLINFO;

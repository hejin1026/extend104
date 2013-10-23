
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

-define(FRAME_TESTFR_R, #extend104_frame{c1=16#43}).
-define(FRAME_TESTFR_T, #extend104_frame{c1=16#83}).

%<<68, 16#0E, 00, 00, 06, 00, 64, 01, 06, 00, 01, 00, 00, 00, 00, 14>>),
-define(FRAME_C_IC_NA_1, #extend104_frame{c3=6, payload = <<16#64, 01, 06, 00, 01, 00, 00, 00, 00, 16#14>>}).

-define(FRAME_C_CI_NA_1, #extend104_frame{c3=2, payload = <<16#65, 01, 06, 00, 01, 00, 00, 00, 00, 16#14>>}).

-define(FRAME_C_CS_NA_1, #extend104_frame{c3=2, payload = <<16#67, 01, 06, 00, 01, 00, 00, 00, 00, 16#14>>}).





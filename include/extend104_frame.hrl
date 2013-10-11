
-record(extend104_frame, {
		c1 = 0		:: integer(),
		c2 = 0		:: integer(),
		c3 = 0		:: integer(),
		c4 = 0 		:: integer(),
		payload	= <<>> :: binary()}).

-record(extend104_asdu, {
		type, 
		vsq,
		sq,
		cot,
		addr,
		data}).

-define(FRAME_STARTDT, #extend104_frame{c1=7}).

-define(FRAME_STOPDT, #extend104_frame{c1=19}).

%<<68, 16#0E, 00, 00, 06, 00, 64, 01, 06, 00, 01, 00, 00, 00, 00, 14>>),
-define(FRAME_C_IC_NA_1, #extend104_frame{c3=6, payload = <<64, 01, 06, 00, 01, 00, 00, 00, 00, 14>>}).



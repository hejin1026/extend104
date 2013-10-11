

-record(extend104_frame, {
		length = 4	:: integer(),
		c1 = 0		:: integer(),
		c2 = 0		:: integer(),
		c3 = 0		:: integer(),
		c4 = 0 		:: integer(),
		payload	= <<>> :: binary()}).

-define(FRAME_STARTDT, #extend104_frame{c1=7}).

-define(FRAME_STOPDT, #extend104_frame{c1=19}).

%<<68, 16#0E, 00, 00, 06, 00, 64, 01, 06, 00, 01, 00, 00, 00, 00, 14>>),
-define(FRAME_C_IC_NA_1, #extend104_frame{length=16#0E, c3=6, payload = <<64, 01, 06, 00, 01, 00, 00, 00, 00, 14>>}).


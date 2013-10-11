-module(extend104_frame).

-include("extend104_frame.hrl").

-export([parse/1,
		serialise/1]).

parse(<<16#68, L, C1, C2, C3, C4, Payload/binary>>) ->
	{ok, #extend104_frame{length=L, c1=C1, c2=C2, c3=C3, c4=C4, payload=Payload}};
	
parse(Bad) ->
	{error, {badframe, Bad}}.

serialise(Frame) when is_record(Frame, extend104_frame) ->
	#extend104_frame{length=L, c1=C1, c2=C2, c3=C3, c4=C4, payload=Payload} = Frame,
	<<16#68, L, C1, C2, C3, C4, Payload/binary>>.



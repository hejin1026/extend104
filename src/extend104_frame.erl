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

-include("extend104_frame.hrl").

-export([parse/1,
		serialise/1]).

parse(<<16#68, L, C1, C2, C3, C4, Payload/binary>>) ->
	PayloadLen = L - 4,
	case size(Payload) == PayloadLen of
	true ->
		{ok, #extend104_frame{c1=C1, c2=C2, c3=C3, c4=C4, payload=Payload}};
	false ->
		{error, {badlen, PayloadLen, Payload}}
	end;
	
parse(BadData) ->
	{error, {badframe, BadData}}.

serialise(Frame) when is_record(Frame, extend104_frame) ->
	#extend104_frame{c1=C1, c2=C2, c3=C3, c4=C4, payload=Payload} = Frame,
	Len = size(Payload) + 4,
	<<16#68, Len, C1, C2, C3, C4, Payload/binary>>.


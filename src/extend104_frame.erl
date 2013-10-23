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

-include_lib("elog/include/elog.hrl").

-export([check_frame/2,
		parse/1,
		serialise/1]).
	
check_frame(<<>>, _Fun) -> <<>>;		
check_frame(<<16#68,L,Data/binary>> = Frames, Fun) ->
	if L =< size(Data) ->
		{Frame, Rest} = split_binary(Data, L),
		Fun(Frame),
		check_frame(Rest, Fun);	
	true ->
		Frames
	end;		
check_frame(Rest, _Fun) ->
	?ERROR("get error msg:~p",[Rest]), 
	<<>>.	

parse(<<C1, C2, C3, C4, Payload/binary>>) ->
	#extend104_frame{c1=C1, c2=C2, c3=C3, c4=C4, payload=Payload}.

serialise(Frame) when is_record(Frame, extend104_frame) ->
	#extend104_frame{c1=C1, c2=C2, c3=C3, c4=C4, payload=Payload} = Frame,
	Len = size(Payload) + 4,
	<<16#68, Len, C1, C2, C3, C4, Payload/binary>>.


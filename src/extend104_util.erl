-module(extend104_util).

-create("hejin 2013-10-18").

-export([reverse_byte/1]).

reverse_byte(Bin) ->
	reverse_byte(Bin, []).

reverse_byte(<<>>, Acc) ->
	list_to_binary(Acc);
reverse_byte(<<One:8,Other/binary>>, Acc) ->
	reverse_byte(Other, [One|Acc]).
	
	

	
	
	
	
	

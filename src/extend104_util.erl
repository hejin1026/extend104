-module(extend104_util).

-create("hejin 2013-10-18").

-export([reverse_byte/1, add_cn/1, bin_to_str/2]).

reverse_byte(Bin) ->
	reverse_byte(Bin, []).

reverse_byte(<<>>, Acc) ->
	list_to_binary(Acc);
reverse_byte(<<One:8,Other/binary>>, Acc) ->
	reverse_byte(Other, [One|Acc]).
	
add_cn({L, H}) ->
	N = (H bsl 8) + L + 2,
	L1 = N band 16#FF,
	H1 = N bsr 8,
	{L1, H1}.	
	
bin_to_str(Bin,N) ->
	bin_to_str(Bin,N, []).

bin_to_str(<<>>, _N, Acc) ->
	string:join(lists:reverse(Acc), " ");
bin_to_str(<<D,Bin/binary>>,N, Acc) ->	 	
	bin_to_str(Bin, N, [zeropad(integer_to_list(D, N))|Acc]).
	
zeropad(I) when length(I) == 1 ->
    lists:concat(["0", I]);
zeropad(I) ->
    I.	

	
	
	
	
	

-module(extend104_util).

-create("hejin 2013-10-18").

-export([reverse_int_value/1,reverse_int_value/2, reverse_byte_value/1, reverse_byte_value2/1, reverse_byte/1, 
		add_cn/1, bin_to_str/2, to_string/1, decode_key/1]).

-export([b2a/1]).


%int -> overturn byte
reverse_int_value(Int) ->
	reverse_int_value(Int, 2).
	
reverse_int_value(Int, N) ->
	reverse_int_value(Int, N, []).
	
reverse_int_value(Int, 0, Acc) ->
	list_to_binary(Acc);	
	
reverse_int_value(Int, N, Acc) ->	
	N1 = N - 1,
	B = (Int bsr (N1 * 8)) band 16#ff,
	reverse_int_value(Int, N1, [B|Acc]).

%overturn byte -> int
reverse_byte_value(Bin) ->
	BitLen = size(Bin) * 8,
	<<V:BitLen>> = reverse_byte(Bin),
	V.

%overturn byte -> +/- int
reverse_byte_value2(Bin) ->
	BitLen = size(Bin) * 8,
	Data = <<V:BitLen>> = reverse_byte(Bin),
	<<T:1,_:7, _/binary>> = Data,
	if T==1 ->
		V - (1 bsl BitLen);
	true -> 
		V
	end.	

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

b2a(Binary) ->
  	list_to_atom(binary_to_list(Binary)).
	
to_string(T)  ->
    lists:flatten(io_lib:format("~p", [T])).	
	
decode_key(Key) ->
	string:tokens(Key, ":").
	
	
	

%%%----------------------------------------------------------------------
%%% Created	: 2014-7-11
%%% author 	: hejin1026@gmail.com
%%%----------------------------------------------------------------------
-module(pavilion_ctl).

-compile(export_all).
	
cluster_info() ->
    Nodes = [node()|nodes()],
    io:format("cluster nodes: ~p~n", [Nodes]).

cluster(Node) ->
	case net_adm:ping(list_to_atom(Node)) of
	pong ->
		io:format("cluster with ~p successfully.~n", [Node]);
	pang ->
        io:format("failed to cluster with ~p~n", [Node])
	end.
	
state(Module) ->
	sys:get_status(list_to_atom(Module)).	
	
	

status() ->
    {InternalStatus, _ProvidedStatus} = init:get_status(),
    io:format("Node ~p is ~p.", [node(), InternalStatus]),
    case lists:keysearch(pavilion, 1, application:which_applications()) of
	false ->
		io:format("pavilion is not running~n");
	{value,_Version} ->
		io:format("pavilion is running~n")
    end.
		
	
	
	
%%%----------------------------------------------------------------------
%%% File    : agent_ctl.erl
%%% Author  : Ery Lee <ery.lee@gmail.com>
%%% Purpose : Opengoss node control
%%% Created : 15 Jan 2010
%%% License : http://www.opengoss.com
%%%
%%% Copyright (C) 2010, www.opengoss.com
%%%----------------------------------------------------------------------
-module(agent_ctl).

-author('ery.lee@gmail.com').

-export([start/0, init/0, process/1]).  

-include_lib("elog/include/elog.hrl").

-define(STATUS_SUCCESS, 0).

-define(STATUS_ERROR,   1).

-define(STATUS_USAGE,   2).

-define(STATUS_BADRPC,  3).

start() ->
    case init:get_plain_arguments() of
	[SNode | Args] ->
	    Node = node_name(SNode),
	    Status = case rpc:call(Node, ?MODULE, process, [Args]) of
			 {badrpc, Reason} ->
			     ?PRINT("RPC failed on the node ~p: ~p~n",
				       [Node, Reason]),
			     ?STATUS_BADRPC;
			 S ->
			     S
		     end,
	    halt(Status);
	_ ->
	    io:format("you make a mistake, wash and go to bed", []),
	    halt(?STATUS_USAGE)
    end.

init() ->
	ok.

node_name(SNode) ->
    SNode1 = 
    case string:tokens(SNode, "@") of
    [_Node, _Server] ->
        SNode;
    _ ->
        case net_kernel:longnames() of
         true ->
             SNode ++ "@" ++ inet_db:gethostname() ++
                  "." ++ inet_db:res_option(domain);
         false ->
             SNode ++ "@" ++ inet_db:gethostname();
         _ ->
             SNode
         end
    end,
    list_to_atom(SNode1).

process(["nodes"]) ->
    ?PRINT("Nodes: ~p ~n", [nodes() -- [node_name("agent_ctl")]]),
    ?STATUS_SUCCESS;

process(["status"]) ->
    {InternalStatus, ProvidedStatus} = init:get_status(),
    ?PRINT("Node ~p is ~p. Status: ~p~n",
              [node(), InternalStatus, ProvidedStatus]),
    case lists:keysearch(agent, 1, application:which_applications()) of
        false ->
            ?PRINT("agent is not running~n", []),
            ?STATUS_ERROR;
        {value,_Version} ->
            ?PRINT("agent is running~n", []),
            ?STATUS_SUCCESS
    end;

process(["cluster", SNode]) ->
    case agent:cluster(node_name(SNode)) of
    {error, Reason}-> 
        ?PRINT("failed to cluster with ~p, reason: ~p", [SNode, Reason]),
        ?STATUS_ERROR;
    {ok, Nodes} ->
        ?PRINT("cluster success, nodes: ~p~n", [Nodes]),
        ?STATUS_SUCCESS
    end;

process(["stop"]) ->
	agent_app:stop(),
    init:stop(),
    ?STATUS_SUCCESS;

process(["restart"]) ->
    init:restart(),
    ?STATUS_SUCCESS;

process(_) -> 
    io:format("you make a mistake, wash and go to bed", []),
    ?STATUS_ERROR.


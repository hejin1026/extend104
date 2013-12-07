%%%----------------------------------------------------------------------
%%% Created	: 2013-12-4
%%% author 	: hejin1026@gmail.com
%%%----------------------------------------------------------------------
-module(master_ctl).

-include_lib("elog/include/elog.hrl").
-include("terminl.hrl").

-export([run/0]).

run() ->
	Dispatch = fun(Cid) ->
		case term:lookup(Cid) of
		{ok, #channel{cid=Cid, data=Data}} ->
			master_dist:dispatch({monitor, Cid, Data});
		{false, _} -> 
			?ERROR("~p is not found", [Cid])
		end
	end,
	spawn(fun() -> 
		AllCid = term:all_channel(),
		?INFO("begin to dispatch ~p entries...", [length(AllCid)]),
		try
			lists:foreach(Dispatch, AllCid)
		catch
			_:Err -> ?ERROR("dispatch error: ~p", [Err])
		end
	end).


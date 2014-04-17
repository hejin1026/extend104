%%%----------------------------------------------------------------------
%%% Created	: 2013-12-4
%%% author 	: hejin1026@gmail.com
%%%----------------------------------------------------------------------
-module(extend104_ctl).

-include_lib("elog/include/elog.hrl").

-compile(export_all).

status() ->
    {InternalStatus, _ProvidedStatus} = init:get_status(),
    ?PRINT("Node ~p is ~p.", [node(), InternalStatus]),
    case lists:keysearch(extend104, 1, application:which_applications()) of
	false ->
		?PRINT_MSG("node is not running~n");
	{value,_Version} ->
		?PRINT_MSG("node is running~n")
    end.
	
state(Type) ->
    io:format("state:~p", [sys:get_status(list_to_atom(Type))]).

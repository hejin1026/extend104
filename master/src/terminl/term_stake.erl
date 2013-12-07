%%%----------------------------------------------------------------------
%%% Created	: 2013-12-6
%%% author 	: hejin1026@gmail.com
%%%----------------------------------------------------------------------
-module(term_stake).

-include("terminl.hrl").
-include_lib("elog/include/elog.hrl").

-term_boot_load({stake, load, "loading stake channel", station}).

-export([load/0]).

load() ->
	Sql = "select t1.*,t2.id as tid from channel t1 left join term_stake t2 on t2.id=t1.stake_id  
           where t1.channel_type =0",
	case emysql:sqlquery(Sql) of
        {ok, Records} ->
            Store = fun(Channel) -> mnesia:write(term:get_channel(stake, Channel)) end,
            mnesia:sync_dirty(fun lists:foreach/2, [Store, Records]),
            ?ERROR("finish ~p: ~p ~n", [?MODULE, length(Records)]),
            {ok, state};
        {error, Reason}  ->
            ?ERROR("start failure...~p",[Reason]),
            {stop, Reason}
	end.

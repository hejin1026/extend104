%%%----------------------------------------------------------------------
%%% Created	: 2013-12-4
%%% author 	: hejin1026@gmail.com
%%%----------------------------------------------------------------------
-module(term_station).

-include("terminl.hrl").
-include_lib("elog/include/elog.hrl").

-term_boot_load({station, load, "loading station channel", undefined}).

-export([load/0]).

load() ->
	Sql = "select t1.*,t2.id as tid from channel t1 left join term_station t2 on t2.id=t1.station_id  
           where t1.channel_type =0",
	case emysql:sqlquery(Sql) of
        {ok, Records} ->
            Store = fun(Channel) -> mnesia:write(term:get_channel(station, Channel)) end,
            mnesia:sync_dirty(fun lists:foreach/2, [Store, Records]),
            ?ERROR("finish ~p: ~p ~n", [?MODULE, length(Records)]),
            {ok, state};
        {error, Reason}  ->
            ?ERROR("start failure...~p",[Reason]),
            {stop, Reason}
	end.



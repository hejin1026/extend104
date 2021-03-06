%%%----------------------------------------------------------------------
%%% Author  : hejin1026@gmail.com
%%% Purpose : supervisor for connection
%%% Created : 2013 10 30
%%%----------------------------------------------------------------------
-module(extend104_connection_sup).

-author("hejin1026@gmail.com/13-10-30").

-behaviour(supervisor2).

-export([start_link/0, start_connection/2]).
-export([init/1]).
	
	
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_connection(Sup, ConnectionParams) ->
    supervisor:start_child(Sup, [self(), ConnectionParams]).	

init([]) ->
    {ok, {{simple_one_for_one, 0, 1},
          [{extend104_connection, {extend104_connection, start_link, []},
           temporary, brutal_kill, worker, [extend104_connection]}]}}.	
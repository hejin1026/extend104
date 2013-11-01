-module(extend104).

-include("extend104.hrl").

-include_lib("elog/include/elog.hrl").

-export([start_link/1,
		open_connection/1,
		get_conn_pid/1
		]).

-behavior(gen_server).

-export([init/1,
		handle_call/3,
		handle_cast/2,
        handle_info/2,
        code_change/3,
        terminate/2]).


-record(state, {connection_sup, map_oid_pid = dict:new()}).

start_link(ConnectionSup) ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [ConnectionSup], []).
	
open_connection(ConnConf) ->
	gen_server:call(?MODULE, {open_connection, ConnConf}).	
	
get_conn_pid(Oid) ->
	gen_server:call(?MODULE, {get_conn_pid, Oid}).		

init([ConnectionSup]) ->
	{ok, #state{connection_sup = ConnectionSup}}.
	
handle_connect(ConnConf, #state{connection_sup = ConnSup, map_oid_pid=MapOP} = State) ->
    case extend104_connection_sup:start_connection(ConnSup, ConnConf) of
        {ok, ConnPid} ->
            Oid = get_oid(ConnConf), 
			{reply, {ok, ConnPid}, State#state{map_oid_pid=dict:store(Oid, ConnPid, MapOP)}};
        {error, Error} ->
            ?ERROR("get conn error: ~p, ~p", [Error, ConnConf]),
			{reply, {error, Error}, State}
     end.	
	
	
handle_call({open_connection, ConnConf}, _From, State) ->
	handle_connect(ConnConf, State);	
handle_call({get_conn_pid, Oid}, _From, #state{map_oid_pid = MapOP} = State) ->
	{reply, dict:find(Oid, MapOP), State};	
handle_call(Req, _From, State) ->
    ?WARNING("unexpect request: ~p", [Req]),
    {reply, {error, {invalid_request, Req}}, State}.
	
handle_cast(Msg, State) ->
	{noreply, State}.
	
handle_info(Msg ,State) ->
	{noreply, State}.
				

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

	
get_oid(ConnConf) ->
	Ip = proplists:get_value(ip, ConnConf),	
	Port = proplists:get_value(port, ConnConf),
	#extend104_oid{ip=extbif:to_binary(Ip), port=Port}.
	
	
	
	
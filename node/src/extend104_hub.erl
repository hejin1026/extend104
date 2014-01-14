%%%----------------------------------------------------------------------
%%% Author  : hejin1026@gmail.com
%%% Purpose : http send packet
%%% Created : 2013-12-9
%%%----------------------------------------------------------------------
-module(extend104_hub).

-include("extend104.hrl").
-include_lib("elog/include/elog.hrl").

-export([start_link/1,
        send_datalog/1,
		stop/0]).

-behavior(gen_server).

-export([init/1,
        handle_call/3,
        handle_cast/2,
        handle_info/2,
        terminate/2,
        code_change/3]).

-record(state, {channel, ertdb}).

-import(extbif, [to_atom/1]).

%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link(Config) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Config], []).

send_datalog(DataList) ->
    gen_server:cast(?MODULE, DataList).


stop() ->
    gen_server:call(?MODULE, stop).

init([Config]) ->
    ?INFO("Monet hub is starting...[done]", []),
    {ok, Conn} = amqp:connect(),
    Channel = open(Conn),
	{ok, Client} = ertdb_client:start_link(Config),
    {ok, #state{channel = Channel, ertdb=Client}}.

open(Conn) ->
    {ok, Channel} = amqp:open_channel(Conn),
    Channel.

handle_call(stop, _From, State) ->
    {stop, normal, ok, State};

handle_call(Req, _From, State) ->
	?ERROR("badreq: ~p", [Req]),
    {reply, {badreq, Req}, State}.

handle_cast({measure, Cid, DateTime, DataList}, #state{ertdb=Client} = State) ->
    ?INFO("send datalog: ~p, ~p",[Cid, DataList]),
    % amqp:send(Channel, <<"measure.datalog">>, term_to_binary(Payload)),
	lists:foreach(fun(Meas) ->
		% MeasId = #measure_id{cid=Cid, type=Meas#measure.type, no=Meas#measure.no},
		Key = build_key(Cid, Meas#measure.type, Meas#measure.no),
		Cmd = ["insert", Key, DateTime, Meas#measure.value],
		ok = ertdb_client:q_noreply(Client, Cmd)
	end, DataList),
    {noreply, State};

handle_cast(Msg, State) ->
    ?ERROR("Unexpected message: ~p", [Msg]),
    {noreply, State}.

handle_info(Info, State) ->
    ?WARNING("unext info :~p", [Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------
build_key(Cid, Type, No) ->
	list_to_binary(lists:concat([Cid, ":", Type, ":", No])).

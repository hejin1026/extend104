%%%----------------------------------------------------------------------
%%% Created	: 2013-12-4
%%% author 	: hejin1026@gmail.com
%%%----------------------------------------------------------------------
-module(extend104_monitor).

-include_lib("elog/include/elog.hrl").

-export([start_link/1]).

-behavior(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3 ]).

-record(state, {channel}).

-import(extbif, [to_binary/1, to_list/1]).


start_link(Opts) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Opts], []).


init([CityId]) ->
    {ok, Conn} = amqp:connect(),
    Channel = open(Conn, CityId),
	?INFO("~p is starting...", [?MODULE]),
	{ok, #state{channel = Channel}}.

open(Conn, CityId) ->
    {ok, Channel} = amqp:open_channel(Conn),
    CityName = CityId ++ ".monitor",
    {ok,CityQ} = amqp:queue(Channel, CityName),
    {ok,MonetQ} = amqp:queue(Channel, get_monet_query()),
    amqp:consume(Channel, CityQ),
    amqp:consume(Channel, MonetQ),
    Channel.

get_monet_query() ->
    [NodeName|_] = string:tokens(atom_to_list(node()), "@"),
    NodeName ++ ".monitor".


handle_call(Req, _From, State) ->
    ?ERROR("Unexpected request: ~p", [Req]),
    {reply, ok, State}.

handle_cast({datalog, DataLog}, #state{channel = Channel} = State) ->
    amqp:send(Channel, <<"master.monet">>, term_to_binary({datalog, DataLog})),
    {noreply, State};

handle_cast(Msg, State) ->
    ?ERROR("Unexpected message: ~p", [Msg]),
    {noreply, State}.


handle_info({deliver, RoutingKey, _Header, Payload}, #state{channel = Channel} = State) ->
    % ?INFO("get from quene :~p", [binary_to_list(RoutingKey)]),
    case binary_to_term(Payload) of
        {monitor, Cid, Data} -> % CityId
            Node = {monitored, Cid, get_monet_query()},
            amqp:send(Channel, <<"monitor.reply">>, term_to_binary(Node)),
            extend104:open_conn(Data);
        {unmonitor, Cid} ->
            % unmonitor(Dn);
			ok;
        _ ->
            ignore
    end,
    {noreply, State};


handle_info({'EXIT', Pid, Reason}, State) ->
	?ERROR("~p monitor exited: ~p,~p", [Pid, node(Pid), Reason]),
    {noreply, State};

handle_info(Info, State) ->
    ?ERROR("unext info :~p", [Info]),
    {noreply, State}.


terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

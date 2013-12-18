%%%----------------------------------------------------------------------
%%% Created	: 2013-12-9
%%% author 	: hejin1026@gmail.com
%%%----------------------------------------------------------------------
-module(master_datalog).

-behavior(gen_server).

-include_lib("elog/include/elog.hrl").
-include("extend104.hrl").

-export([start_link/0,
		get_measure/1
		]).

-export([init/1, 
		handle_call/3, 
		handle_cast/2, 
		handle_info/2, 
		terminate/2, 
		code_change/3]).

-record(state, {channel}).

-import(extbif, [to_binary/1]).
	
start_link() ->
    gen_server2:start_link({local, ?MODULE}, ?MODULE, [], []).
	
get_measure(Measure) ->
	gen_server2:call(?MODULE, {get_measure, Measure}).		
		
init([]) ->
    {ok, Conn} = amqp:connect(),
    Channel = open(Conn),
	ets:new(extend104_measure, [ordered_set, named_table, {keypos, #measure.id}]),
    {ok, #state{channel = Channel}}.

open(Conn) ->
    {ok, Channel} = amqp:open_channel(Conn),
    amqp:queue(Channel, <<"measure.datalog">>),
    amqp:consume(Channel, <<"measure.datalog">>, self()),
    Channel.

handle_call({get_measure, {Cid, MeaType, MeaNo}}, _From, State) ->
	Meas = case MeaType of
		'$_' ->
			ets:tab2list(extend104_measure);
		_ ->	
			ets:match_object(extend104_measure, 
				#measure{id= #measure_id{cid=binary_to_integer(Cid), type=binary_to_integer(MeaType),no=binary_to_integer(MeaNo)},
					 station_no='_', cot='_',value='_'})
	end,			
	{reply, {ok, Meas}, State};
	
handle_call(Msg, _From, State) ->
	{reply, ok, State}.
	
	
handle_cast(Msg, State) ->
	{noreply, State}.
	
handle_info({deliver, <<"measure.datalog">>, _Properties, Payload}, State) ->
    ?INFO("get monitor datalog :~p", [binary_to_term(Payload)]),
    handle_reply(binary_to_term(Payload), State),
    {noreply, State};
	
handle_info(Msg, State) ->
	?ERROR("unexpected info: ~p", [Msg]),
	{noreply, State}.
	
	
terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

handle_reply({measure, Cid, Datalist}, _State) ->
	lists:foreach(fun(Meas) ->
		MeasId = Meas#measure.id,
		NewMeasId = MeasId#measure_id{cid=Cid},
		ets:insert(extend104_measure, Meas#measure{id=NewMeasId})
	end, Datalist);		
handle_reply(_Reply, _State) ->
    ok.		
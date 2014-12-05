%% The contents of this file are subject to the Mozilla Public License
%% Version 1.1 (the "License"); you may not use this file except in
%% compliance with the License. You may obtain a copy of the License at
%% http://www.mozilla.org/MPL/
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See the
%% License for the specific language governing rights and limitations
%% under the License.
%%
%% The Original Code is eMQTT
%%
%% The Initial Developer of the Original Code is <ery.lee at gmail dot com>
%% Copyright (C) 2012 Ery Lee All Rights Reserved.

-module(emqtt_client).

-behaviour(gen_server2).

-export([
		subscribe/2,
		unsubscribe/2,
		consume/2,
		publish/2]).

-export([start_link/1, go/2, info/1, update/2]).

-export([init/1,
		handle_call/3,
		handle_cast/2,
		handle_info/2,
        code_change/3,
		terminate/2]).

-include("emqtt.hrl").

-include("emqtt_frame.hrl").

-include("emqtt_internal.hrl").

-include("elog.hrl").

-define(TCPOPTIONS, [binary, {packet, raw}, {active, true}]).

% -define(TIMEOUT, 8000).

-define(CLIENT_ID_MAXLEN, 23).

-define(PROTOCOL_MAGIC, "MQIsdp").

-record(state, {host,port,
				socket,
				conn_name,
				await_recv,
				connection_state,
				parse_state,
                message_id,
                client_id,
				keep_alive, 
				awaiting_ack,
                subtopics,
				awaiting_rel,
				consumer,
				msg,
				ref}).


-define(FRAME_TYPE(Frame, Type),
        Frame = #mqtt_frame{ fixed = #mqtt_frame_fixed{ type = Type }}).

start_link(Args) ->
    gen_server2:start_link(?MODULE, [Args], []).

go(Pid, Sock) ->
	gen_server2:call(Pid, {go, Sock}).
	
update(Pid, Args) ->
	gen_server2:cast(Pid, {update, Args}).	
	
subscribe(Client, {Topic, Qos}) when is_pid(Client) ->
	gen_server2:call(Client, {subscribe, {Topic, Qos}}).

unsubscribe(Client, Topic) when is_list(Topic) and is_pid(Client) ->
	gen_server2:cast(Client, {unsubscribe, Topic}).
	
consume(Client, Consumer) ->
	gen_server2:call(Client, {consume, Consumer}).
	
publish(Client, {Topic, Qos, Payload}) ->
	gen_server2:cast(Client, {publish, {Topic, Qos, Payload}}).



info(Pid) ->
	gen_server2:call(Pid, info).

init([Args]) ->
	Addr = proplists:get_value(ip, Args),
	Port = proplists:get_value(port, Args),
	Cid = proplists:get_value(id, Args),
	case do_connect(extbif:to_list(Cid), extbif:to_list(Addr), Port) of
		{ok, State} ->
			process_flag(trap_exit, true),
			{ok, State};
		{error, Reason} ->
			{stop, {connection_error, Reason}}
	end.

do_connect(Cid, Addr, Port) ->
    case gen_tcp:connect(Addr, Port, ?TCPOPTIONS) of
        {ok, Sock}     -> 
			Fixed =  #mqtt_frame_fixed{type 	 = ?CONNECT},
			KeepAlive = 300,
			VariableBin = <<0, 6,?PROTOCOL_MAGIC,3,2#11000010,KeepAlive:16/big>>,	
			?INFO("get cid:~p", [Cid]),	
			Len = length(Cid),
			Payload = list_to_binary([<<Len:16/big>>,Cid, 0,4, "root", 0, 6, "public"]),
			Frame = emqtt_frame:serialise_fixed(Fixed, VariableBin, Payload),
			send_frame(Sock, Frame),
			{ok, #state{host = Addr, port = Port,
						socket           = Sock,
		                await_recv       = false,
						keep_alive		 = KeepAlive,
						client_id 		 = Cid,
		                connection_state = running,
						message_id		 = 1,
						parse_state      = emqtt_frame:initial_state(),
						consumer		 = [],
						msg				 = dict:new()
				}};
        {error, Reason} = E -> 
			?ERROR("connect fail:~p, reason:~p", [{Cid, Addr, Port}, Reason]),
			E
    end.

handle_call(info, _From, #state{conn_name=ConnName, 
	message_id=MsgId, client_id=ClientId} = State) ->
	Info = [{conn_name, ConnName},
			{message_id, MsgId},
			{client_id, ClientId}],
	{reply, Info, State};
	   
			   
handle_call({subscribe, {Topic, QoS}}, _Form, State=#state{socket = Sock, message_id=MsgId}) ->
	Fixed =  #mqtt_frame_fixed{type = ?SUBSCRIBE, qos = QoS},
	VariableBin =  #mqtt_frame_subscribe{message_id=MsgId, topic_table=[#mqtt_topic{name = Topic, qos = QoS}]},
	
	Frame = emqtt_frame:serialise(#mqtt_frame{fixed=Fixed, variable=VariableBin, payload= <<>> }),
	send_frame(Sock, Frame),
	{reply, ok, State};
	
handle_call({unsubscribe, {Topic, QoS}}, _Form, State) ->
	
	
	{reply, ok, State};	
	
handle_call({consume, Consumer}, _Form, #state{consumer = CS}=State) ->
	?INFO("get consume:~p, ~p", [Consumer, CS]),
	{reply, ok, State#state{consumer = [Consumer|CS]}};
	
handle_call(Msg, _Form, State) ->
	{stop, {badmsg, Msg}, State}.	
			   

handle_cast({publish, {Topic, Qos, Payload}}, #state{socket = Sock, message_id=MsgId, msg=Msg} = State) ->
	?INFO("publish :~p", [{Topic, Qos, Payload}]),
	Frame = #mqtt_frame{
		fixed = #mqtt_frame_fixed{type 	 = ?PUBLISH,
								  qos    = Qos},
		variable = #mqtt_frame_publish{topic_name = Topic,
									   message_id = if
													Qos == ?QOS_0 -> undefined;
													true -> MsgId
													end},
		payload = term_to_binary(Payload)},

	send_frame(Sock, Frame),
	State1 = State#state{msg = dict:store(MsgId, Frame, Msg)},
	if
	Qos == ?QOS_0 ->
		{noreply, State1};
	true ->
		{noreply, next_msg_id(State1)}
	end;

		   
handle_cast({update, Args}, State) ->
	%TODO
	{noreply, State};
	
handle_cast(Msg, State) ->
	{stop, {badmsg, Msg}, State}.		

handle_info({inet_reply, _Ref, ok}, State) ->
    {noreply, State, hibernate};

handle_info({tcp, Sock, Data}, #state{ socket = Sock}=State) ->
    process_received_bytes(Data, run_socket(State #state{ await_recv = false }));


handle_info(heartbeat, #state{socket=Sock, keep_alive=KeepAlive, connection_state=running}=State) ->
	send_frame(Sock, #mqtt_frame{fixed = #mqtt_frame_fixed{ type = ?PINGREQ }}),
	?INFO("after send heartbeat...~p", [KeepAlive]),
    erlang:send_after(KeepAlive*1000, self(), heartbeat),
    {noreply, State};
	
handle_info(reconnect, #state{host=Addr, port=Port, client_id = Cid}=State) ->
	case do_connect(Cid, Addr, Port) of
		{ok, NewState} ->
			{noreply, NewState};
		{error, Reason} ->
			{stop, Reason, State}
	end;	

handle_info({tcp_closed, Sock}, #state{socket=Sock, client_id=ClientId} = State) ->
	?ERROR("tcp closed:~p", [ClientId]),
	% reconnect
	erlang:send_after(3000, self(), reconnect),
    {noreply, State#state{connection_state = blocked}};

handle_info(Info, State) ->
	?ERROR("unext info:~p", [Info]),
	{noreply, State}.

terminate(_Reason, #state{client_id=ClientId, keep_alive=KeepAlive}) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
	
throw_on_error(E, Thunk) ->
    case Thunk() of
	{error, Reason} -> throw({E, Reason});
	{ok, Res}       -> Res;
	Res             -> Res
    end.

async_recv(Sock, Length, infinity) when is_port(Sock) ->
    prim_inet:async_recv(Sock, Length, -1);

async_recv(Sock, Length, Timeout) when is_port(Sock) ->
    prim_inet:async_recv(Sock, Length, Timeout).

%-------------------------------------------------------
% receive and parse tcp data
%-------------------------------------------------------
process_received_bytes(<<>>, State) ->
    {noreply, State};

process_received_bytes(Bytes,
                       State = #state{ parse_state = ParseState,
                                       conn_name   = ConnStr }) ->
	?INFO("recv ~p~n", [Bytes]),
    case emqtt_frame:parse(Bytes, ParseState) of
	{more, ParseState1} ->
		?INFO("get more....", []),
		{noreply,
		 run_socket( State #state{ parse_state = ParseState1 }),
		 hibernate};
	{ok, Frame, Rest} ->
		?INFO("get frame:~p", [Frame]),
		case process_frame(Frame, State) of
		{ok, State1} ->
			PS = emqtt_frame:initial_state(),
			process_received_bytes(
			  Rest,
			  State1 #state{ parse_state = PS});
		{err, Reason, State1} ->
			?ERROR("MQTT protocol error ~p for connection ~p~n", [Reason, ConnStr]),
			stop({shutdown, Reason}, State1);
		{stop, State1} ->
			stop(normal, State1)
		end;
	{error, Error} ->
		?ERROR("MQTT detected framing error ~p for connection ~p~n", [ConnStr, Error]),
		stop({shutdown, Error}, State)
    end.



process_frame(Frame = #mqtt_frame{fixed = #mqtt_frame_fixed{type = Type}},
              State=#state{client_id=ClientId, keep_alive=KeepAlive}) ->
	?INFO("frame from ~s: ~p, ~p", [ClientId, Type, Frame]),
	handle_retained(Type, Frame),
	process_request(Type, Frame, State),
	{ok, State}.


process_request(?CONNACK, Frame=#mqtt_frame{variable = #mqtt_frame_connack{
                                         return_code = ReturnCode }}, #state{keep_alive=KeepAlive, ref=Ref}=State) ->
	?INFO("get connack...",[]),										 
	cancel_timer(Ref),
	erlang:send_after(KeepAlive*1000, self(), heartbeat),
	{ok, State};

process_request(?PUBLISH, Frame=#mqtt_frame{
									fixed = #mqtt_frame_fixed{qos = ?QOS_0},
									payload = Payload}, State) ->
	?INFO("get publish payload:~p", [binary_to_term(Payload)]),
	handle_payload(binary_to_term(Payload), State),									
	{ok, State};

process_request(?PUBLISH,
                Frame=#mqtt_frame{
                  fixed = #mqtt_frame_fixed{qos    = ?QOS_1},
                  variable = #mqtt_frame_publish{message_id = MsgId},
				  payload = Payload}, 
				State=#state{socket=Sock}) ->
	?INFO("get msg:~p, ~p", [MsgId, Payload]),
	handle_payload(binary_to_term(Payload), State),	
	{ok, State};

process_request(?PUBLISH,
                Frame=#mqtt_frame{
                  fixed = #mqtt_frame_fixed{qos    = ?QOS_2},
                  variable = #mqtt_frame_publish{message_id = MsgId}}, 
				State=#state{socket=Sock}) ->
	
	send_frame(Sock, #mqtt_frame{fixed = #mqtt_frame_fixed{type = ?PUBREC},
			  variable = #mqtt_frame_publish{ message_id = MsgId}}),
	{ok, State};

process_request(?PUBACK, Frame=#mqtt_frame{
				variable = #mqtt_frame_publish{message_id = MsgId}}, 
				#state{msg = Msg} = State) ->
	{ok, State#state{msg = dict:erase(MsgId, Msg)}};

process_request(?PUBREC, #mqtt_frame{
	variable = #mqtt_frame_publish{message_id = MsgId}}, 
	State=#state{socket=Sock}) ->
	%TODO: fixme later
	send_frame(Sock,
	  #mqtt_frame{fixed    = #mqtt_frame_fixed{ type = ?PUBREL},
				  variable = #mqtt_frame_publish{ message_id = MsgId}}),
	{ok, State};

process_request(?PUBREL,
                #mqtt_frame{
                  variable = #mqtt_frame_publish{message_id = MsgId}},
				  State=#state{socket=Sock}) ->
	send_frame(Sock,
	  #mqtt_frame{fixed    = #mqtt_frame_fixed{ type = ?PUBCOMP},
				  variable = #mqtt_frame_publish{ message_id = MsgId}}),
	{ok, State};

process_request(?PUBCOMP, #mqtt_frame{
	variable = #mqtt_frame_publish{message_id = _MsgId}}, State) ->
	%TODO: fixme later
	{ok, State};

process_request(?SUBSCRIBE,
                #mqtt_frame{
                  variable = #mqtt_frame_subscribe{message_id  = MessageId,
                                                   topic_table = Topics},
                  payload = undefined},
                #state{socket=Sock} = State) ->

	[emqtt_router:subscribe({Name, Qos}, self()) || 
			#mqtt_topic{name=Name, qos=Qos} <- Topics],

    GrantedQos = [Qos || #mqtt_topic{qos=Qos} <- Topics],

    send_frame(Sock, #mqtt_frame{fixed = #mqtt_frame_fixed{type = ?SUBACK},
								 variable = #mqtt_frame_suback{
											 message_id = MessageId,
											 qos_table  = GrantedQos}}),

    {ok, State};
	
process_request(?SUBACK, #mqtt_frame{
                  variable = #mqtt_frame_suback{message_id  = MessageId,
                                                   qos_table = QosTable },
                  payload = undefined}, State) ->
	%TODO				  
	{ok, State};
					  					  

process_request(?UNSUBSCRIBE,
                #mqtt_frame{
                  variable = #mqtt_frame_subscribe{message_id  = MessageId,
                                                   topic_table = Topics },
                  payload = undefined}, #state{socket = Sock, client_id = ClientId} = State) ->

	
	[emqtt_router:unsubscribe(Name, self()) || #mqtt_topic{name=Name} <- Topics], 

    send_frame(Sock, #mqtt_frame{fixed = #mqtt_frame_fixed{type = ?UNSUBACK },
                             	 variable = #mqtt_frame_suback{message_id = MessageId }}),

    {ok, State};

process_request(?PINGRESP, Frame, State) ->
    {ok, State}.
	
	

handle_payload({measure, Datalist}, State=#state{consumer = CS, client_id=Cid}) ->
	DateTime = extbif:timestamp(),
	[ C ! {measure, Cid, DateTime, Datalist} || C <- CS].


next_msg_id(State = #state{ message_id = 16#ffff }) ->
    State #state{ message_id = 1 };
next_msg_id(State = #state{ message_id = MsgId }) ->
    State #state{ message_id = MsgId + 1 }.

maybe_clean_sess(false, _Conn, _ClientId) ->
    % todo: establish subscription to deliver old unacknowledged messages
    ok.

%%----------------------------------------------------------------------------

make_will_msg(#mqtt_frame_connect{ will_flag   = false }) ->
    undefined;
make_will_msg(#mqtt_frame_connect{ will_retain = Retain,
                                   will_qos    = Qos,
                                   will_topic  = Topic,
                                   will_msg    = Msg }) ->
    #mqtt_msg{retain  = Retain,
              qos     = Qos,
              topic   = Topic,
              dup     = false,
              payload = Msg }.

send_frame(Sock, Frame) when is_record(Frame, mqtt_frame)->
	?INFO("send :~p", [Frame]),
    erlang:port_command(Sock, emqtt_frame:serialise(Frame));
send_frame(Sock, Bin) ->
	erlang:port_command(Sock, Bin).
	

%%----------------------------------------------------------------------------
network_error(Reason,
              State = #state{ conn_name  = ConnStr}) ->
    ?INFO("MQTT detected network error '~p' for ~p", [Reason, ConnStr]),
    % todo: flush channel after publish
    stop({shutdown, conn_closed}, State).

run_socket(State = #state{ connection_state = blocked }) ->
    State;
run_socket(State = #state{ await_recv = true }) ->
    State;
run_socket(State = #state{ socket = Sock }) ->
    async_recv(Sock, 0, infinity),
    State#state{ await_recv = true }.

stop(Reason, State ) ->
    {stop, Reason, State}.

valid_client_id(ClientId) ->
    ClientIdLen = length(ClientId),
    1 =< ClientIdLen andalso ClientIdLen =< ?CLIENT_ID_MAXLEN.

handle_retained(?PUBLISH, #mqtt_frame{fixed = #mqtt_frame_fixed{retain = false}}) ->
	ignore;

handle_retained(?PUBLISH, #mqtt_frame{
                  fixed = #mqtt_frame_fixed{retain = true},
                  variable = #mqtt_frame_publish{topic_name = Topic},
				  payload= <<>> }) ->
	emqtt_retained:delete(Topic);

handle_retained(?PUBLISH, Frame=#mqtt_frame{
                  fixed = #mqtt_frame_fixed{retain = true},
                  variable = #mqtt_frame_publish{topic_name = Topic}}) ->
	emqtt_retained:insert(Topic, make_msg(Frame));

handle_retained(_, _) -> 
	ignore.

make_msg(#mqtt_frame{
			  fixed = #mqtt_frame_fixed{qos    = Qos,
										retain = Retain,
										dup    = Dup},
			  variable = #mqtt_frame_publish{topic_name = Topic,
											 message_id = MessageId},
			  payload = Payload}) ->
	#mqtt_msg{retain     = Retain,
			  qos        = Qos,
			  topic      = Topic,
			  dup        = Dup,
			  message_id = MessageId,
			  payload    = Payload}.
			  
cancel_timer(undefined) -> ok;
cancel_timer(Ref) -> erlang:cancel_timer(Ref).

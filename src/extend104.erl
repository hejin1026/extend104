-module(extend104).

-include("extend104.hrl").

-include("extend104_frame.hrl").

-export([start_link/1,
		status/1,
		send/2]).

-behavior(gen_fsm).

%% gen_fsm callbacks
-export([init/1,
        handle_info/3,
        handle_event/3,
        handle_sync_event/4,
        code_change/4,
        terminate/3]).

% fsm state
-export([connecting/2,
        connecting/3,
        connected/2,
        connected/3,
		disconnected/2]).

-define(TCPOPTIONS, [
		binary,
		{packet,    raw},
		{reuseaddr, true},
		{backlog,   128},
		{nodelay,   true},
		{active,    true},
		{reuseaddr, true}]).

-define(TIMEOUT, 8000).

-record(state, {host, port, sock, msgid}).

start_link(Args) ->
	gen_fsm:start_link(?MODULE, [Args], []).

status(C) when is_pid(C) ->
	gen_fsm:sync_send_all_state_event(C, status).

-spec send(C :: pid(), Cmd :: 'STARTDT' | 'STOPDT' | 'C_IC_NA_1' | atom()) -> ok.
send(C, Cmd) when is_pid(C) and is_atom(Cmd) ->
	gen_fsm:send_event(C, Cmd).

init([Args]) ->
	Host = proplists:get_value(host, Args),
	Port = proplists:get_value(port, Args),
	{ok, connecting, #state{host=Host, port=Port}, 0}.

connecting(timeout, State) ->
    connect(State);

connecting(_Event, State) ->
    {next_state, connecting, State}.

connecting(_Event, _From, State) ->
    {reply, {error, connecting}, connecting, State}.

connect(State = #state{host=Host, port=Port}) ->
    case gen_tcp:connect(Host, Port, ?TCPOPTIONS, ?TIMEOUT) of
    {ok, Sock} ->
		io:format("~p:~p is connected.~n", [Host, Port]),
        {next_state, connected, State#state{sock = Sock}};
    {error, Reason} ->
		io:format("failed to connect ~p:~p, error: ~p.~n", [Host, Port, Reason]),
		erlang:send_after(30000, self(), reconnect),
        {next_state, connecting, State#state{sock = undefined}}
    end.

connected('STARTDT', State=#state{sock = Sock}) ->
	send_frame(Sock, ?FRAME_STARTDT),
	{next_state, connected, State};

connected('STOPDT', State=#state{sock = Sock}) ->
	send_frame(Sock, ?FRAME_STOPDT),
	{next_state, connected, State};

connected('C_IC_NA_1', State=#state{sock = Sock}) ->
	send_frame(Sock, ?FRAME_C_IC_NA_1),
	{next_state, connected, State};

connected(Event, State) ->
	io:format("badevent: ~p~n", [Event]),
	{next_state, connected, State}.

connected(_Event, _From, State) ->
    {reply, {error, badevent}, connected, State}.

disconnected(connect, State) ->
	connect(State);

disconnected(Event, State) ->
	io:format("badevent ~p~n", [Event]),
	{next_state, discconnected, State}.

handle_info({tcp, _Sock, Data}, connected, State) ->
	case extend104_frame:parse(Data) of
	{ok, Frame} -> 
		process_frame(Frame);
	{error, Error} -> 
		io:format("Error: ~p~n", [Error])
	end,
    {next_state, connected, State};

handle_info({tcp_closed, Sock}, connected, State=#state{sock=Sock}) ->
	io:format("tcp closed...~n"),
    {next_state, disconnected, State};

handle_info({timeout, reconnect}, connecting, S) ->
    connect(S);

handle_info(_Info, StateName, State) ->
    {next_state, StateName, State}.

handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.

handle_sync_event(status, _From, StateName, State) ->
    %Statistics = [{N, get(N)} || N <- [inserted]],
    {reply, StateName, StateName, State};

handle_sync_event(stop, _From, _StateName, State) ->
    {stop, normal, ok, State}.

terminate(_Reason, _StateName, _State) ->
    ok.

code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

send_frame(Sock, Frame) when is_binary(Frame) ->
    erlang:port_command(Sock, Frame);
	
send_frame(Sock, Frame) ->
    erlang:port_command(Sock, extend104_frame:serialise(Frame)).

process_frame(Frame) ->
	io:format("Received: ~p~n", [Frame]).





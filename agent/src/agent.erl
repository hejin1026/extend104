-module(agent).

-created("hejin 2012-5-4").

-include_lib("elog/include/elog.hrl").

-export([start_link/0, stop/0]).

-behavior(gen_server).

-export([init/1, 
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3 ]).

-record(state, {channel}).

%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
    gen_server:call(?MODULE, stop).

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([]) ->
    process_flag(trap_exit, true),
    {ok, Conn} = amqp:connect(),
    Channel = open(Conn),
    erlang:send_after(6 * 1000, self(), tick),
    ?INFO("Agent is started...[ok]", []),
    {ok, #state{channel = Channel}}.

open(Conn) ->
    {ok, Channel} = amqp:open_channel(Conn),
     Channel.


handle_call(stop, _From, State) ->
    {stop, normal, ok, State};

handle_call(Req, _From, State) ->
    ?ERROR("badreq: ~p", [Req]),
    {reply, {badreq, Req}, State}.

handle_cast(Msg, State) ->
    ?ERROR("badmsg: ~p", [Msg]),
    {noreply, State}.

handle_info(tick, #state{channel = Channel} = State) ->
    WorkerNum = length(nodes()),
    SID = atom_to_list(node()),
    try 
       {unix, OsType} = os:type(),
       Mod = case OsType of
           'hp-ux' -> mon_hpux;
           'aix' -> mon_aix;
           _ -> mon_unix
       end,
       Mod:run([{sid, SID}])  of
        {ok, HostInfo,  Datalogs} ->
            ?INFO("~p", [Datalogs]),
			[_AgentName, HostName] = string:tokens(SID, "@"),
            HostInfo1 = [{sid, node()},{host_name, HostName}, {worker_num, WorkerNum} | HostInfo],
            ?INFO("hostinfo: ~p", [HostInfo1]),
            amqp:send(Channel, <<"agent.reply">>, term_to_binary({hostinfo, HostInfo1})),
            amqp:send(Channel, <<"server.datalog">>, term_to_binary(Datalogs))
    catch
         _:Exception -> ?ERROR("~p, ~p", [Exception, erlang:get_stacktrace()])
    end,
    erlang:send_after(300 * 1000, self(), tick),
    {noreply, State};

handle_info(Info, State) ->
    ?ERROR("badinfo: ~p", [Info]),
    {noreply, State}.

terminate(_Reason, #state{channel = Channel}) ->
    amqp:stop(Channel).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.



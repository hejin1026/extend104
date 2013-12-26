%%%----------------------------------------------------------------------
%%% Created	: 2013-12-20
%%% author 	: hejin1026@gmail.com
%%%----------------------------------------------------------------------
-module(term_agent).

-include_lib("elog/include/elog.hrl").

-behaviour(gen_server).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {channel}).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([]) ->
    {ok, Conn} = amqp:connect(),
    Channel = open(Conn),
    io:format("finish start ~p...~n",[?MODULE]),
    {ok, #state{channel = Channel}}.

open(Conn) ->
    {ok, Channel} = amqp:open_channel(Conn),
    amqp:queue(Channel, <<"term.reply">>),
    amqp:consume(Channel, <<"term.reply">>),
    Channel.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({deliver, <<"term.reply">>, _Properties, Payload}, State) ->
    handle_datalist(binary_to_term(Payload)),
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

handle_datalist(DataList) when is_list(DataList) ->
    ?INFO("get disco data :~p",[DataList]),
	lists:foreach(fun(Data) -> ?INFO("get disco data :~p",[Data]),
        handle_data(Data)
    end, DataList);
handle_datalist(Data)  ->
    handle_data(Data).

handle_data({update, Table,Data}) ->
    try update_data(Table,Data)
    catch _:Err ->?ERROR("bad error: ~p,~p,~p", [Err,Table, Data])
    end;

handle_data(Data) ->
    ?INFO("unkown data :~p", [Data]),
    ok.

update_data(Table,Data) ->
    case emysql:update(Table, [{updated_at, {datetime, calendar:local_time()}} | Data]) of
        {updated, {1, _}} -> ?INFO("insert data: ~p,~p", [Table,Data]);
        {updated, {0, _}} -> ?WARNING("stale data: ~p,~p", [Table,Data]);
        {error, Reason} ->  ?ERROR("~p,~p,~p", [Table,Data,Reason])
    end.


insert_data(Table,Data) ->
    case emysql:insert(Table, [{created_at, {datetime, calendar:local_time()}} | Data]) of
        {updated, {1, _}} -> ?INFO("insert data: ~p,~p", [Table,Data]);
        {updated, {0, _}} -> ?WARNING("stale data: ~p,~p", [Table,Data]);
        {error, Reason} -> ?ERROR("~p,~p,~p", [Table,Data,Reason])
    end.



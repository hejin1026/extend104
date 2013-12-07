%%%----------------------------------------------------------------------
%%% Created	: 2013-12-4
%%% author 	: hejin1026@gmail.com
%%%----------------------------------------------------------------------
-module(term).

-include_lib("elog/include/elog.hrl").

-behavior(gen_server).

-export([start_link/0]).

-export([get_channel/2,all_channel/0, lookup/1, module_with_attrs/2]).

-export([init/1, 
		handle_call/3, 
		handle_cast/2, 
		handle_info/2, 
		terminate/2, 
		code_change/3]).
		
-record(channel, {cid, ip, port, tid, type, data}).		


all_channel() ->
	mnesia:dirty_all_keys(channel).
	
lookup(Cid) ->
	case mnesia:dirty_read(channel, Cid) of
    [Channel] ->
        {ok, Channel};
    [] ->
        false
    end.
	
start_link() ->
	gen_server2:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    {atomic, ok} = mnesia:create_table(channel,
        [{ram_copies, [node()]}, {index, [ip]},
         {attributes, record_info(fields, channel)}]),

    BootSteps = [{Name, Mod, Fun, Descr, Dep}
                        || {Mod, [{Name, Fun, Descr, Dep}]}
                            <- module_with_attrs(master, term_boot_load)],
    [put({boot_step, element(1, Step)}, Step) || Step <- BootSteps],
    [boot_load_step(Step) || Step <- BootSteps],
	{ok, state}.
	
boot_load_step({Name, Mod, Fun, Descr, Dep}) ->
	case get({boot_load, Name}) of
	true ->
		ok;
	_ ->
		DepLoaded = get({boot_load, Dep}),
        ?ERROR("begin to load ~s", [Name]),
		if
		Dep =:= undefined ->
			ok;
		DepLoaded ->
			ok;
		true ->
			boot_load_step(get({boot_step, Dep}))
		end,
		?ERROR("end with load ~p, ~s", [Name, Descr]),
		Mod:Fun(),
		put({boot_load, Name}, true)
	end.	

handle_call(Msg, _From, State) ->
	{reply, ok, State}.
	
handle_cast(Msg, State) ->
	{noreply, State}.
	
handle_info(Msg, State) ->
	{noreply, State}.
	
	
terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
		

%copy from rabbitmq
module_with_attrs(App, Name) ->
	case application:get_key(App, modules) of
	{ok, Modules} ->
		lists:foldl(
		  fun (Module, Acc) ->
			  case lists:append([Atts || {N, Atts} <- module_attributes(Module),
										 N =:= Name]) of
			  []   -> Acc;
			  Atts -> [{Module, Atts} | Acc]
			  end
		  end, [], Modules);
	undefined ->
		[]
	end.

%copy from rabbitmq
module_attributes(Module) ->
    case catch Module:module_info(attributes) of
	{'EXIT', Reason} ->
		exit(Reason);
	V ->
		V
    end.


get_channel(Type, Channel) ->
	Id = proplists:get_value(id, Channel),
	Ip = proplists:get_value(ip, Channel),
	Port = proplists:get_value(port, Channel),
	Tid = proplists:get_value(tid, Channel),
	#channel{cid=Id, ip=Ip, port=Port, tid=Tid, type=Type, data=Channel}.	
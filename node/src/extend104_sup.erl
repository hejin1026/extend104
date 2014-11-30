
-module(extend104_sup).

-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

-include_lib("elog/include/elog.hrl").

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link(CityId) ->
    {ok, Sup} = supervisor:start_link({local, ?MODULE}, ?MODULE, []),
    ConnectionSub = {extend104_connection_sup, {extend104_connection_sup, start_link, []},
        	temporary, infinity , supervisor, [extend104_connection_sup]},
    {ok, ConnSup} = supervisor:start_child(Sup, ConnectionSub),
	
    Extend104 = {extend104, {extend104, start_link, [CityId, ConnSup]},
			permanent, 10, worker, [extend104]},
    supervisor:start_child(Sup, Extend104),
    Monitor = {extend104_monitor, {extend104_monitor, start_link, [CityId]},
			permanent, 10, worker, [extend104_monitor]},
	supervisor:start_child(Sup, Monitor),
	
    Monitor2 = {extend104_monitor2, {extend104_monitor2, start_link, []},
			permanent, 10, worker, [extend104_monitor2]},
	supervisor:start_child(Sup, Monitor2),
	
	{ok, Config} = application:get_env(ertdb), 
    Hub = {extend104_hub, {extend104_hub, start_link, [Config]},
			permanent, 10, worker, [extend104_hub]},
	supervisor:start_child(Sup, Hub),
	{ok, Sup}.

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    {ok, { {one_for_one, 5, 10}, []} }.


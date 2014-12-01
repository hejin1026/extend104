
-module(inter_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-include_lib("elog/include/elog.hrl").

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 10, Type, [I]}).
-define(CHILD(I, Type, Args), {I, {I, start_link, [Args]}, permanent, 10, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).
	

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
	Inter = ?CHILD(inter, worker),
   
	{ok, HttpdConf} = application:get_env(httpd), 
    HttpdCowBoy = ?CHILD(inter_httpd, worker, HttpdConf),
    {ok, { {one_for_one, 5, 10}, [Inter, HttpdCowBoy]} }.


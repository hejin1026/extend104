%%%----------------------------------------------------------------------
%%% Created	: 2014-7-11
%%% author 	: hejin1026@gmail.com
%%%----------------------------------------------------------------------
-module(pavilion_sup).

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
	Pavilion = ?CHILD(pavilion, worker),
	
	{ok, HttpdConf} = application:get_env(httpd), 
    HttpdCowBoy = {pavilion_httpd, {pavilion_httpd, start, [HttpdConf]},
			permanent, 10, worker, [pavilion_httpd]},
    {ok, { {one_for_one, 5, 10}, [Pavilion, HttpdCowBoy]} }.


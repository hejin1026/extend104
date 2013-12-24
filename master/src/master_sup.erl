%%%----------------------------------------------------------------------
%%% Created	: 2013-12-4
%%% author 	: hejin1026@gmail.com
%%%----------------------------------------------------------------------
-module(master_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-include_lib("elog/include/elog.hrl").

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 10, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).
	

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
	Matser = ?CHILD(master, worker),
	Dist = ?CHILD(master_dist, worker),
	Term = ?CHILD(term, worker),
	TermAgent = ?CHILD(term_agent, worker),
	{ok, HttpdConf} = application:get_env(httpd), 
    HttpdCowBoy = {master_httpd, {master_httpd, start, [HttpdConf]},
			permanent, 10, worker, [master_httpd]},
	Datalog = ?CHILD(master_datalog, worker),		
    {ok, { {one_for_one, 5, 10}, [Matser, Dist, Term, TermAgent, HttpdCowBoy, Datalog]} }.


-module(agent_sup).

-behavior(supervisor).

-export([start_link/0, init/1]).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
	Agent = {agent, {agent, start_link, []},
		permanent, 10, worker, [agent]},

	{ok, {{one_for_one, 2, 100}, [Agent]}}.


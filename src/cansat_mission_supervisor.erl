-module(cansat_mission_supervisor).

-behaviour(supervisor).

-export([init/1, start_link/1]).

start_link(Options) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE,
			  Options).

init(_Options) ->
    Flags = {one_for_one, 1000, 3600},
    Specs = [{cansat_mission_worker,
	      {cansat_mission_worker, start_link, []}, permanent, 2000,
	      worker, [cansat_mission_worker]}],
    {ok, {Flags, Specs}}.

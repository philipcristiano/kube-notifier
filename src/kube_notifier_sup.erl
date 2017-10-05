-module(kube_notifier_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    ConfigDirs = application:get_env(kube_notifier, config_dirs, []),

	Procs = [
        #{id => kube_notifier_watcher,
          start => {kube_notifier_watcher, start_link, []},
          modules => [kube_notifier_watcher]
        },
        #{id => kube_notifier_config,
          start => {kube_notifier_config, start_link, [ConfigDirs]},
          modules => [kube_notifier_config]
        }
    ],
	{ok, {{one_for_one, 1, 5}, Procs}}.

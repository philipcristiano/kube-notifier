-module(kube_notifier_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    AppSlackChannel = application:get_env(kube_notifier, slack_channel, "#general"),
    SlackChannel = os:getenv("SLACK_CHANNEL", AppSlackChannel),

    AppSlackToken = application:get_env(kube_notifier, slack_token, undefined),
    SlackToken = os:getenv("SLACK_TOKEN", AppSlackToken),

    Options = [{slack_channel, SlackChannel}, {slack_token, SlackToken}],
	Procs = [
        #{id => kube_notifier_watcher,
          start => {kube_notifier_watcher, start_link, [Options]},
          modules => [kube_notifier_watcher]
        }
    ],
	{ok, {{one_for_one, 1, 5}, Procs}}.

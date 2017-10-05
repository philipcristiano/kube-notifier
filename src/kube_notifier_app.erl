-module(kube_notifier_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->

    ConfigDirs = application:get_env(kube_notifier, config_dirs, []),
    kube_notifier_config:read_config_dirs(ConfigDirs),
    validate_config(kube_notifier, [slack_token]),

	kube_notifier_sup:start_link().

stop(_State) ->
	ok.

validate_config(_App, []) ->
    ok;
validate_config(App, [H|T]) ->
    Val = application:get_env(App, H),
    ErrorMsg = lists:flatten(io_lib:format("Setting not defined ~s", [H])),
    case Val of
        undefined -> {error, ErrorMsg};
        _ -> validate_config(App, T)
    end.

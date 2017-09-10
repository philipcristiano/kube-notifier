PROJECT = kube_notifier
PROJECT_DESCRIPTION = New project
PROJECT_VERSION = 0.0.1

DEPS = lager swaggerl kuberlnetes slacker
LOCAL_DEPS = sasl inets ssl

SHELL_OPTS = -eval 'application:ensure_all_started(kube_notifier).' -config sys

dep_lager = git https://github.com/erlang-lager/lager.git 3.4.1
dep_swaggerl = git https://github.com/philipcristiano/swaggerl.git master
dep_kuberlnetes = git https://github.com/philipcristiano/kuberlnetes.git master
dep_slacker = git https://github.com/julienXX/slacker.git v0.4


include erlang.mk

PROJECT = kube_notifier
PROJECT_DESCRIPTION = New project
PROJECT_VERSION = 0.0.1

DEPS = lager swaggerl kuberlnetes slacker
LOCAL_DEPS = sasl inets ssl

SHELL_OPTS = -eval 'application:ensure_all_started(kube_notifier).' -config sys

dep_lager = git https://github.com/erlang-lager/lager.git 3.4.1
dep_swaggerl = git https://github.com/philipcristiano/swaggerl.git debug
dep_kuberlnetes = git https://github.com/philipcristiano/kuberlnetes.git in-cluster
dep_slacker = git https://github.com/julienXX/slacker.git v0.4


include erlang.mk

docker_image:
	docker build . -t kube-notifier:build -f Dockerfile.build

docker_release: docker_image
	docker run --name build kube-notifier:build
	docker cp build:/kube-notifier/_rel docker_rel

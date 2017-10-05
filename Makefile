PROJECT = kube_notifier
PROJECT_DESCRIPTION = New project
PROJECT_VERSION = 0.0.1

DEPS = lager swaggerl kuberlnetes slacker fs
LOCAL_DEPS = sasl inets ssl

SHELL_OPTS = -eval 'application:ensure_all_started(kube_notifier).' -config sys

dep_lager = git https://github.com/erlang-lager/lager.git 3.4.1
dep_swaggerl = git https://github.com/philipcristiano/swaggerl.git debug
dep_kuberlnetes = git https://github.com/philipcristiano/kuberlnetes.git in-cluster
dep_slacker = git https://github.com/julienXX/slacker.git v0.4
dep_fs = git https://github.com/synrc/fs.git 2.12.0


include erlang.mk

docker_image:
	docker build . -t kube-notifier:build -f Dockerfile.build

docker_release: docker_image
	docker rm build || true
	docker run --name build kube-notifier:build
	docker cp build:/kube-notifier/_rel docker_rel

local_image: docker_release
	docker build . -f Dockerfile.assemble -t kube-notifier:latest

PROJECT = bm

PKG_REVISION ?= $(shell git describe --tags)
PKG_VERSION	?= $(shell git describe --tags | tr - .)
ELEVELDB_VERSION = "1.1.0"
export ELEVELDB_VERSION

SHELL_OPTS = -eval "application:ensure_all_started(bm)" -config bm


DEPS = leveltsdb cowboy jsx erlydtl emqttc
dep_leveltsdb = git https://github.com/philipcristiano/leveltsdb.git 0.1.4
dep_cowboy = git https://github.com/ninenines/cowboy 1.0.0
dep_jsx = git https://github.com/talentdeficit/jsx.git v2.1.1
dep_erlydtl = git https://github.com/erlydtl/erlydtl.git 0.10.0
dep_emqttc = git https://github.com/emqtt/emqttc.git v0.2.1-beta


.PHONY: release

release: clean app
	./relx release

package: release
	fpm -s dir -t deb -n iot-bm -v 0.1.0 _rel/bm=/opt/ rel/init=/etc/init.d/iot-bm

include erlang.mk

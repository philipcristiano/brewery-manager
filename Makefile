PROJECT = bm

PKG_REVISION ?= $(shell git describe --tags)
PKG_VERSION	?= $(shell git describe --tags | tr - .)
ELEVELDB_VERSION = "1.1.0"
export ELEVELDB_VERSION

SHELL_OPTS = -eval "application:ensure_all_started(bm)" -config bm
PLT_APPS = ranch

DEPS = leveltsdb lager cowboy jsx erlydtl
dep_leveltsdb = git https://github.com/philipcristiano/leveltsdb.git 0.1.4
dep_cowboy = git https://github.com/ninenines/cowboy 1.0.2
dep_jsx = git https://github.com/talentdeficit/jsx.git v2.7.0
dep_erlydtl = git https://github.com/erlydtl/erlydtl.git 0.10.0
dep_lager = git https://github.com/basho/lager.git 3.0.1


.PHONY: release

release: clean app
	./relx release

package: app rel
	fpm -s dir -t deb -n iot-bm -v 0.1.0 _rel/bm=/opt/ rel/init=/etc/init.d/iot-bm

export IPS_FMRI=server/${PROJECT}@${PKG_VERSION}
export IPS_DESCRIPTION="Brewery Manager"
export IPS_SUMMARAY="${IPS_DESCRIPTION}"

ips_package: app rel
	./omnios-build/generate_pkg_mog.sh
	pkgsend generate _rel/${PROJECT} | pkgfmt > omnios-build/pkg.pm5.1


include erlang.mk

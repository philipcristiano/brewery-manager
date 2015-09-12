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

BUILD_TIME=$(shell TZ=UTC date +"%Y%m%dT%H%M%SZ")
export IPS_FMRI=server/${PROJECT}@$(shell git describe --tags --abbrev=0):${BUILD_TIME}
export IPS_DESCRIPTION="Brewery Manager"
export IPS_SUMMARAY=${IPS_DESCRIPTION}
PKG_VERSION	?= $(shell git describe --tags | tr - .)
ARCH=$(shell uname -p)

define IPS_METADATA
set name=pkg.fmri value=${IPS_FMRI}
set name=pkg.description value=${IPS_DESCRIPTION}
set name=pkg.summary value=${IPS_SUMMARAY}
set name=variant.arch value=${ARCH}
endef
export IPS_METADATA

ips_package: app rel
	# Create metadata
	echo "$$IPS_METADATA" > omnios-build/pkg.mog
	# Get file data for the release
	pkgsend generate _rel/${PROJECT} | pkgfmt > omnios-build/pkg.pm5.1
	# Combine file and metadata
	pkgmogrify omnios-build/pkg.pm5.1 omnios-build/pkg.mog | pkgfmt > omnios-build/pkg.pm5.2
	# Lint package
	pkglint omnios-build/pkg.pm5.2


include erlang.mk

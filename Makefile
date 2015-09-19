PROJECT = bm

PKG_REVISION ?= $(shell git describe --tags)
PKG_VERSION	?= $(shell git describe --tags | tr - .)
UNAME ?=$(shell uname)
ELEVELDB_VERSION = "1.1.0"
export ELEVELDB_VERSION

RELX_OPTS = --overlay_vars=rel/overlay_vars/${UNAME}.config
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

BUILD_TMP?=omnios-build
BUILD_DIR?=omnios-build/root

ips_package: app rel
	# Create metadata
	echo "$$IPS_METADATA" > ${BUILD_TMP}/pkg.mog

	rm -rf ${BUILD_DIR}
	mkdir -p ${BUILD_DIR}/opt

	cp -r _rel/${PROJECT} ${BUILD_DIR}/opt

	echo "<transform dir path=(etc|opt)$ -> drop>" > ${BUILD_TMP}/transform.mog

	# Get file data for the release
	pkgsend generate ${BUILD_DIR} | pkgfmt > ${BUILD_TMP}/pkg.pm5.1
	# Combine file and metadata
	pkgmogrify ${BUILD_TMP}/pkg.pm5.1 ${BUILD_TMP}/pkg.mog ${BUILD_TMP}/transform.mog | pkgfmt > ${BUILD_TMP}/pkg.pm5.2
	cp ${BUILD_TMP}/pkg.pm5.2 ${BUILD_TMP}/pkg.pm5.final
	# Lint package
	pkglint ${BUILD_TMP}/pkg.pm5.final

ips_publish: ips_package
ifndef PKGSRVR
	echo "Need to define PKGSRVR, something like http://localhost:10000"
	exit 1
endif
	pkgsend publish -s ${PKGSRVR} -d ${BUILD_DIR} ${BUILD_TMP}/pkg.pm5.final


include erlang.mk

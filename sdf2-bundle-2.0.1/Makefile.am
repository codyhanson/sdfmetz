SUBDIRS    = . $(PKGS)
EXTRA_DIST = sdf2-bundle.spec.in sdf2-bundle.spec svn-revision

DUMMIES=\
  pre-pkgs/build/toolbuslib.dummy \
  pre-pkgs/build/pt-support.dummy \
  pre-pkgs/build/asf-support.dummy \
  pre-pkgs/build/asc-support.dummy \
  pre-pkgs/build/sdf-support.dummy \
  pre-pkgs/build/sglr.dummy

install-exec-local: ${DUMMIES}
all-local: ${DUMMIES}

BUILD_TB_LIB=@abs_top_srcdir@/pre-pkgs/build
BUILD_PT_SUPPORT=@abs_top_srcdir@/pre-pkgs/build
BUILD_ASC_SUPPORT=@abs_top_srcdir@/pre-pkgs/build
BUILD_ASF_SUPPORT=@abs_top_srcdir@/pre-pkgs/build
BUILD_SDF_SUPPORT=@abs_top_srcdir@/pre-pkgs/build
BUILD_SGLR=@abs_top_srcdir@/pre-pkgs/build

WITH_BUILD_PT_SUPPORT=--with-pt-support=${BUILD_PT_SUPPORT}
WITH_BUILD_SDF_SUPPORT=--with-sdf-support=${BUILD_SDF_SUPPORT}
WITH_BUILD_TB_LIB=--with-toolbuslib=${BUILD_TB_LIB}
WITH_ATERM=--with-aterm=$(ATERM)

# TODO: remove the dummy on a clean or whatever.
pre-pkgs/build/toolbuslib.dummy :
	mkdir -p pre-pkgs &&\
	mkdir -p pre-pkgs/build &&\
	cp -rfp toolbuslib pre-pkgs/build/ &&\
	cd pre-pkgs/build/toolbuslib &&\
	./configure --prefix=${BUILD_TB_LIB} $(WITH_ATERM) &&\
	$(MAKE) &&\
	$(MAKE) install &&\
	touch @abs_top_srcdir@/pre-pkgs/build/toolbuslib.dummy

pre-pkgs/build/pt-support.dummy :
	cp -rfp pt-support pre-pkgs/build/ &&\
	cd pre-pkgs/build/pt-support &&\
	./configure --prefix=${BUILD_PT_SUPPORT} ${WITH_BUILD_TB_LIB} $(WITH_ATERM) &&\
	$(MAKE) &&\
	$(MAKE) install &&\
	touch @abs_top_srcdir@/pre-pkgs/build/pt-support.dummy

pre-pkgs/build/asc-support.dummy :
	cp -rfp asc-support pre-pkgs/build/ &&\
	cd pre-pkgs/build/asc-support &&\
	./configure --prefix=${BUILD_ASC_SUPPORT} ${WITH_BUILD_PT_SUPPORT} --with-asf-support=${BUILD_ASF_SUPPORT} ${WITH_BUILD_TB_LIB} $(WITH_ATERM) &&\
	$(MAKE) &&\
	$(MAKE) install &&\
	touch @abs_top_srcdir@/pre-pkgs/build/asc-support.dummy

pre-pkgs/build/asf-support.dummy :
	cp -rfp asf-support pre-pkgs/build/ &&\
	cd pre-pkgs/build/asf-support &&\
	./configure --prefix=${BUILD_ASF_SUPPORT} ${WITH_BUILD_PT_SUPPORT} $(WITH_ATERM) &&\
	$(MAKE) &&\
	$(MAKE) install &&\
	touch @abs_top_srcdir@/pre-pkgs/build/asf-support.dummy

pre-pkgs/build/sdf-support.dummy :
	cp -rfp sdf-support pre-pkgs/build/ &&\
	cd pre-pkgs/build/sdf-support &&\
	./configure --prefix=${BUILD_SDF_SUPPORT} ${WITH_BUILD_TB_LIB} ${WITH_BUILD_PT_SUPPORT} $(WITH_ATERM) &&\
	$(MAKE) &&\
	$(MAKE) install &&\
	touch @abs_top_srcdir@/pre-pkgs/build/sdf-support.dummy

pre-pkgs/build/sglr.dummy :
	cp -rfp sglr pre-pkgs/build/ &&\
	cd pre-pkgs/build/sglr &&\
	./configure --prefix=${BUILD_SGLR} ${WITH_BUILD_PT_SUPPORT} ${WITH_BUILD_TB_LIB} $(WITH_ATERM) &&\
	$(MAKE) &&\
	$(MAKE) install &&\
	touch @abs_top_srcdir@/pre-pkgs/build/sglr.dummy

fixeddist : dist
	mv $(PACKAGE_TARNAME)-$(VERSION).tar.gz $(PACKAGE).tar.gz

# $Id: Makefile.am,v 1.9 2003/06/27 10:03:46 jurgenv Exp $

SUBDIRS		= lib utils test

EXTRA_DIST = make_rules $(wildcard *.pkg)

noinst_SCRIPTS = ${PACKAGE}-${VERSION}.pkg

${PACKAGE}-${VERSION}.pkg: ${PACKAGE}.pkg Makefile
	sed 's#__VERSION__#${VERSION}#g;\
             s#__PACKAGE__#${PACKAGE}#g' < $< > $@


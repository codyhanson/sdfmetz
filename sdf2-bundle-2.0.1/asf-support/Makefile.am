# $Id: Makefile.am,v 1.9 2003/06/27 10:01:46 jurgenv Exp $

SUBDIRS		= lib utils

EXTRA_DIST	= program-check.m4 $(wildcard *.pkg)

noinst_SCRIPTS = ${PACKAGE}-${VERSION}.pkg

${PACKAGE}-${VERSION}.pkg: ${PACKAGE}.pkg Makefile
	sed 's#__VERSION__#${VERSION}#g;\
             s#__PACKAGE__#${PACKAGE}#g' < $< > $@

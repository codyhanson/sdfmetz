# $Id: Makefile.am,v 1.15 2003/09/25 11:43:40 jurgenv Exp $

SUBDIRS		= libmept libptmept libasfix2 libconversion utils  test

EXTRA_DIST=$(wildcard *.pkg)

noinst_SCRIPTS = ${PACKAGE}-${VERSION}.pkg

${PACKAGE}-${VERSION}.pkg: ${PACKAGE}.pkg Makefile
	sed 's#__VERSION__#${VERSION}#g;\
             s#__PACKAGE__#${PACKAGE}#g' < $< > $@

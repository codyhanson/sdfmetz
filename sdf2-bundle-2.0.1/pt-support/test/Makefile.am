# $Id: Makefile.am,v 1.2 2003/09/25 12:54:09 jurgenv Exp $

SUBDIRS=terms

TESTS=mept-test

noinst_PROGRAMS=mept-test

mept_test_SOURCES=mept-test.c
mept_test_LDADD=\
	-L$(ATERM)/lib -lATerm\
	-L$(top_builddir)/libmept -lmept\
	-L$(top_builddir)/test/terms -ltestTerms

INCLUDES =\
	-I${top_srcdir}/libmept\
	-I${top_srcdir}/test/terms\
	-I${ATERM}/include

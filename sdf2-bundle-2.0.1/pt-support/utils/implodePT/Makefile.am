# $Id: Makefile.am,v 1.2 2001/12/03 16:23:35 jurgenv Exp $

SUBDIRS=test

bin_PROGRAMS		= implodePT 

INCLUDES		= -I${top_srcdir}/libmept \
			-I${ATERM}/include 


implodePT_SOURCES	= implodePT-main.c
implodePT_LDADD		= -L${top_builddir}/libmept -lmept \
			-L${ATERM}/lib -lATerm

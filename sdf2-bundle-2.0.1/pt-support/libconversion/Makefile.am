# $Id: Makefile.am,v 1.10 2001/11/06 09:12:43 olivierp Exp $

#CFLAGS	+= -DNO_SHARING

lib_LIBRARIES           = libconversion.a
include_HEADERS         = conversion.h
libconversion_a_SOURCES = flattenPT.c inflatePT.c

INCLUDES		= -I${top_srcdir}/libmept \
                          -I${top_srcdir}/libasfix2 \
			  -I${ATERM}/include

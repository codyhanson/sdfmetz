# $Id: Makefile.am,v 1.12 2003/10/09 07:32:18 markvdb Exp $

lib_LIBRARIES		= libPTMEPT.a
include_HEADERS		= PTMEPT.h PTMEPT-utils.h PTMEPT_dict.h 

libPTMEPT_a_SOURCES	= PTMEPT.c PTMEPT_dict.c PTMEPT_bottom.c ${UTILSRCS}
UTILSRCS		= lift.c utils.c

EXTRA_DIST		= AsFix.def

INCLUDES		= -I. \
			  -I${top_srcdir}/libmept \
                          -I${ATERM}/include  

CFLAGS                  += -DDISABLE_DYNAMIC_CHECKING

# Developers only:
api: AsFix.def
	sdf2-to-c -i $< -P PTPT_ -o PTMEPT

api-clean:
	rm -f PTMEPT.[ch] PTMEPT_dict.* PTMEPT.adt PTMEPT_bottom.c

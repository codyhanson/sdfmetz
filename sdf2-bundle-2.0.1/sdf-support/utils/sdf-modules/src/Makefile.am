# $Id: Makefile.am,v 1.12 2003/06/03 07:46:49 jong Exp $

include ${top_srcdir}/make_rules

EXTRA_DIST = sdf-modules.idef
pkgdata_DATA = sdf-modules.idef

bin_PROGRAMS = sdf-modules

sdf_modules_SOURCES = sdf-modules.c sdf-modules.tif.c \
	module-table.c \
	sdf-imports.c \
	plain-imports.c

sdf_modules_LDADD =\
		 -L$(top_builddir)/lib -lSDFME  \
                 -L$(TOOLBUSLIB)/lib -lATB \
                 -L$(ATERM)/lib -lATerm \
                 $(SOCKLIBS)

noinst_HEADERS = sdf-modules.tif.h \
	module-table.h \
	sdf-imports.h \
	plain-imports.h

INCLUDES = \
	-I$(top_srcdir)/lib  \
	-I$(ATERM)/include  \
	-I$(TOOLBUSLIB)/include \
	-I$(SDF_SUPPORT)/include

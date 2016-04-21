# $Id: Makefile.am,v 1.9 2001/07/25 09:55:20 olivierp Exp $

SUBDIRS    = erroneous tables terms

EXTRA_DIST = lang-types regress-terms.sh.src

TESTS      = regress-terms

CLEANFILES = regress-terms

% : %.sh.src 
	sed 's@__ATERM__@$(ATERM)@g;\
	     s@__SRCDIR__@$(srcdir)@g' < $< > $@ && \
	chmod +x $@

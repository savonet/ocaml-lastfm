# OCaml-Lastfm examples.
#
# Copyright 2007 by the Savonet team.
#
# $Id: Makefile 4694 2007-10-23 23:33:11Z smimram $

all clean:
	$(MAKE) -f Makefile.request $@
	$(MAKE) -f Makefile.submit $@
	$(MAKE) -f Makefile.download $@

distclean: clean
	rm -rf autom4te.cache config.log config.status autom4te.cache

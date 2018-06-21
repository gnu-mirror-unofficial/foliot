
####
#### Copyright (C) 2015 - 2017
#### Free Software Foundation, Inc.

#### This file is part of GNU Foliot.

#### GNU Foliot is free software: you can redistribute it and/or
#### modify it under the terms of the GNU General Public License as
#### published by the Free Software Foundation, either version 3 of
#### the License, or (at your option) any later version.

#### GNU Foliot is distributed in the hope that it will be useful, but
#### WITHOUT ANY WARRANTY; without even the implied warranty of
#### MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
#### General Public License for more details.

#### You should have received a copy of the GNU General Public License
#### along with GNU Foliot.  If not, see
#### <http://www.gnu.org/licenses/>.
####


include $(top_srcdir)/am/guile.mk

moddir=$(sitedir)
godir=$(siteccachedir)

SOURCES = \
	foliot/config.scm		\
	foliot/globals.scm		\
	foliot/colours.scm		\
	foliot/db-con.scm		\
	foliot/db-imported-db.scm	\
	foliot/db-foliot.scm		\
	foliot/db-printing-templates.scm	\
	foliot/db-shinning.scm		\
	foliot/db.scm			\
	foliot/iter.scm			\
	foliot/tl-widget.scm		\
	foliot/c-dialog.scm		\
	foliot/connect.scm		\
	foliot/i-dialog.scm		\
	foliot/import.scm		\
	foliot/p-dialog.scm		\
	foliot/p-common.scm		\
	foliot/p-commercial.scm		\
	foliot/p-draft.scm		\
	foliot/p-lvars.scm		\
	foliot/p-main.scm		\
	foliot/print.scm		\
	foliot/foliot.scm

NOCOMP_SOURCES= \
	$(wildcard foliot/glade/*)	\
	$(wildcard foliot/icons/*)	\
	$(wildcard foliot/latex/*)	\
	$(wildcard foliot/gtkrc/*)	\
	$(shell find $(top_srcdir)/foliot/pof -type f)

EXTRA_DIST = \
	AUTHORS				\
	ChangeLog			\
	COPYING				\
	INSTALL				\
	LICENSE				\
	NEWS				\
	README				\
	pre-inst-env.in			\
	foliot/foliot.in		\
	$(wildcard am/*)		\
	$(wildcard m4/*)		\
	$(wildcard foliot/sqlite/*)	\
	$(SOURCES)			\
	$(NOCOMP_SOURCES)

bin_SCRIPTS = foliot/foliot

CLEANFILES = \
	$(GOBJECTS)			\
	$(bin_SCRIPTS)

ETAGS_ARGS = \
	foliot/foliot			\
	$(SOURCES)

SUBDIRS = \
	test-suite	\
	doc

distclean-local:
	rm -f Makefile.in
	rm -f aclocal.m4
	rm -rf autom4te.cache
	rm -rf build-aux
	rm -f configure
	rm -f *~
	rm -f foliot-*.tar.gz
	rm -f foliot-*.tar.gz.sig
	rm -rf doc/foliot
	rm -f doc/*.info
	rm -f doc/*.pdf
	rm -f doc/*~
	rm -f doc/*.aux doc/*.cp doc/*.cps doc/*.fn doc/*.ky
	rm -f doc/*.log doc/*.pg doc/*.toc doc/*.tp doc/*.vr
	rm -f doc/Makefile.in
	rm -f doc/stamp-vti
	rm -f foliot/*~

# * .PHONY: gen-pof:

# Makefile.am ends here

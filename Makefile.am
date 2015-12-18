
####
#### Copyright (C) 2015
#### Free Software Foundation, Inc.

#### This file is part of Kisê.

#### Kisê is free software: you can redistribute it and/or modify it
#### under the terms of the GNU General Public License as published by
#### the Free Software Foundation, either version 3 of the License, or
#### (at your option) any later version.

#### Kisê is distributed in the hope that it will be useful, but
#### WITHOUT ANY WARRANTY; without even the implied warranty of
#### MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
#### General Public License for more details.

#### You should have received a copy of the GNU General Public License
#### along with Kisê.  If not, see <http://www.gnu.org/licenses/>.
####


include $(top_srcdir)/am/guile.mk

moddir=$(ggdir)
godir=$(gccache)

SOURCES = \
	kise/config.scm			\
	kise/globals.scm		\
	kise/colours.scm		\
	kise/db-con.scm			\
	kise/db-imported-db.scm		\
	kise/db-kise.scm		\
	kise/db-printing-templates.scm	\
	kise/db-shinning.scm		\
	kise/db.scm			\
	kise/iter.scm			\
	kise/tl-widget.scm		\
	kise/c-dialog.scm		\
	kise/connect.scm		\
	kise/i-dialog.scm		\
	kise/import.scm			\
	kise/p-dialog.scm		\
	kise/p-common.scm		\
	kise/p-commercial.scm		\
	kise/p-draft.scm		\
	kise/p-lvars.scm		\
	kise/p-main.scm			\
	kise/print.scm			\
	kise/kise.scm

NOCOMP_SOURCES= \
	$(wildcard kise/glade/*)	\
	$(wildcard kise/icons/*)	\
	$(wildcard kise/latex/*)	\
	$(wildcard kise/gtkrc/*)	\
	$(shell find $(top_srcdir)/kise/pof -type f)

EXTRA_DIST = \
	AUTHORS				\
	ChangeLog			\
	COPYING				\
	INSTALL				\
	LICENSE				\
	NEWS				\
	README				\
	env.in				\
	kise/kise.in			\
	meta/guild.in			\
	$(wildcard org/*)		\
	$(wildcard am/*)		\
	$(wildcard m4/*)		\
	$(wildcard kise/sqlite/*)	\
	$(SOURCES)			\
	$(NOCOMP_SOURCES)

bin_SCRIPTS = kise/kise

CLEANFILES = \
	$(GOBJECTS)			\
	$(bin_SCRIPTS)

ETAGS_ARGS = \
	kise/kise			\
	$(SOURCES)

SUBDIRS = \
	doc

distclean-local:
	rm -f Makefile.in
	rm -f aclocal.m4
	rm -rf autom4te.cache
	rm -rf build-aux
	rm -f configure
	rm -f *~
	rm -f kise-*.tar.gz
	rm -f kise-*.tar.gz.sig
	rm -rf doc/kise
	rm -f doc/*.info
	rm -f doc/*.pdf
	rm -f doc/*~
	rm -f doc/*.aux doc/*.cp doc/*.cps doc/*.fn doc/*.ky
	rm -f doc/*.log doc/*.pg doc/*.toc doc/*.tp doc/*.vr
	rm -f doc/Makefile.in
	rm -f doc/stamp-vti
	rm -f kise/*~

# * .PHONY: gen-pof:

# Makefile.am ends here

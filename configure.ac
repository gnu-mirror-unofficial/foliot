dnl -*- Autoconf -*-
dnl
dnl   configuration script for Kisê
dnl   Process this file with autoconf to produce configure.
dnl

define(KISE_CONFIGURE_COPYRIGHT,[[


Copyright (C) 2015 - 2016
Free Software Foundation, Inc.

This file is part of Kisê.

Kisê is free software: you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

Kisê is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with Kisê.  If not, see <http://www.gnu.org/licenses/>.

]])

AC_PREREQ(2.69)

AC_INIT([kise], [0.9.5], [david at altosw dot be])

AC_CONFIG_AUX_DIR([build-aux])
AC_CONFIG_MACRO_DIR([m4])
dnl Use `serial-tests' so the output `check-guile' is not hidden
dnl (`parallel-tests' is the default in Automake 1.13.)
dnl `serial-tests' was introduced in Automake 1.12.
AM_INIT_AUTOMAKE([1.14 gnu -Wall -Werror -Wno-portability -Wno-syntax \
  color-tests serial-tests foreign])
AM_MAINTAINER_MODE([enable])
AM_SILENT_RULES([yes])

dnl sqlite3-pcre
dnl AC_PROG_CC
dnl AC_STDC_HEADERS
dnl LT_INIT([disable-static])

AC_COPYRIGHT(KISE_CONFIGURE_COPYRIGHT)
AC_CONFIG_SRCDIR([kise])

AC_CHECK_PROG(have_makeinfo, makeinfo, yes, no)
AM_CONDITIONAL(HAVE_MAKEINFO, test "$have_makeinfo" = yes)

GUILE_PKG([2.2, 2.0])
GUILE_PROGS([2.0.11])
GUILE_SITE_DIR
GUILE_SITE_CCACHE_DIR
GUILE_MODULE_REQUIRED([gnome-2])

GUILE_GNOME_PROGS([2.16.4])
GUILE_GNOME_FLAGS
GUILE_GNOME_INST_DIR
GUILE_GNOME_LIBS_PATH
dnl guile-gnome is a set of wrappers, we need to check that the user
dnl has wrapped the gnome libraries kise needs.  strictly speaking,
dnl the list below is 'too much' because some guile-gnome modules are
dnl always part of guile-gnome, but hey, no arm and nice to see all
dnl the checks.
GUILE_GNOME_MODULE_REQUIRED([gnome gnome])
GUILE_GNOME_MODULE_REQUIRED([gnome gobject])
GUILE_GNOME_MODULE_REQUIRED([gnome glib])
GUILE_GNOME_MODULE_REQUIRED([gnome gtk])
GUILE_GNOME_MODULE_REQUIRED([gnome gtk gdk-event])
GUILE_GNOME_MODULE_REQUIRED([gnome gnome-ui])
GUILE_GNOME_MODULE_REQUIRED([gnome glade])

# Checks are in Grip_SQLite
PKG_CHECK_MODULES(SQLite3, sqlite3 >= 3.7)
PKG_CHECK_MODULES(libpcre, libpcre)

PKG_CHECK_MODULES([Grip], [grip-1.0 >= 0.1.0])
PKG_CHECK_MODULES([Grip_Gnome], [grip-gnome-1.0 >= 0.1.0])
PKG_CHECK_MODULES([Grip_SQLite], [grip-sqlite-1.0 >= 0.1.0])

AC_CHECK_PROG(PDFLATEX, pdflatex, yes)
if test -z "$PDFLATEX"; then
  AC_MSG_ERROR([pdflatex required but not found])
fi

LATEX_DOCUMENT_CLASS_REQUIRED([extarticle])
LATEX_PACKAGE_REQUIRED([inputenc])
LATEX_PACKAGE_REQUIRED([fontenc])
LATEX_PACKAGE_REQUIRED([lmodern])
LATEX_PACKAGE_AVAILABLE([HAS_IWONA], [iwona])
# LATEX_PACKAGE_REQUIRED([iwona])
LATEX_PACKAGE_REQUIRED([pifont])
LATEX_PACKAGE_REQUIRED([amsfonts])
LATEX_PACKAGE_REQUIRED([amssymb])
LATEX_PACKAGE_REQUIRED([amsmath])
LATEX_PACKAGE_REQUIRED([wasysym])
LATEX_PACKAGE_REQUIRED([graphicx])
LATEX_PACKAGE_REQUIRED([parskip])
LATEX_PACKAGE_REQUIRED([xspace])
LATEX_PACKAGE_REQUIRED([tabularx])
LATEX_PACKAGE_REQUIRED([longtable])
LATEX_PACKAGE_REQUIRED([ltxtable])
LATEX_PACKAGE_REQUIRED([multicol])
LATEX_PACKAGE_REQUIRED([booktabs])
LATEX_PACKAGE_REQUIRED([hhline])
LATEX_PACKAGE_REQUIRED([numprint])
LATEX_PACKAGE_REQUIRED([fancyhdr])
LATEX_PACKAGE_REQUIRED([lastpage])
LATEX_PACKAGE_REQUIRED([xcolor])
LATEX_PACKAGE_REQUIRED([colortbl])
LATEX_PACKAGE_REQUIRED([fancybox])
LATEX_PACKAGE_REQUIRED([textcomp])
LATEX_PACKAGE_REQUIRED([calc])
LATEX_PACKAGE_REQUIRED([geometry])

AC_CONFIG_FILES([env], [chmod +x env])
AC_CONFIG_FILES([meta/guild], [chmod +x meta/guild])
AC_CONFIG_FILES([kise/kise], [chmod +x kise/kise])
AC_CONFIG_FILES([Makefile doc/Makefile])
AC_OUTPUT
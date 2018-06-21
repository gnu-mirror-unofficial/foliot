dnl -*- Autoconf -*-
dnl
dnl   configuration script for GNU Foliot
dnl   Process this file with autoconf to produce configure.
dnl

define(FOLIOT_CONFIGURE_COPYRIGHT,[[


Copyright (C) 2015 - 2018
Free Software Foundation, Inc.

This file is part of GNU Foliot.

GNU Foliot is free software: you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation, either version 3 of the License, or (at your
option) any later version.

GNU Foliot is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Foliot.  If not, see <http://www.gnu.org/licenses/>.

]])

AC_PREREQ(2.69)

AC_INIT(
  [foliot],
  [0.9.8],
  [bug-foliot@gnu.org])

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

AC_COPYRIGHT(FOLIOT_CONFIGURE_COPYRIGHT)
AC_CONFIG_SRCDIR([foliot])

AC_CHECK_PROG(have_makeinfo, makeinfo, yes, no)
AM_CONDITIONAL(HAVE_MAKEINFO, test "$have_makeinfo" = yes)

GUILE_PKG([2.2 2.0])
GUILE_PROGS([2.0.14])
GUILE_SITE_DIR
GUILE_SITE_CCACHE_DIR
GUILE_MODULE_REQUIRED([gnome-2])
f
AC_ARG_WITH(
  [guile-site],
  [AS_HELP_STRING(
     [--with-guile-site],
     [install source modules using Guile-Gnome site dir and compiled files using Guile's site-ccache dir])],
  [case "x$withval" in
     xyes)
     guile_site="yes";;
     *)
     guile_site="";;
   esac],
  [guile_site=""])

AM_CONDITIONAL(
  [USE_GUILE_SITE],
  [test $guile_site = yes])

GUILE_GNOME_PROGS([2.16.5])
GUILE_GNOME_FLAGS
GUILE_GNOME_SITE_DIR
GUILE_GNOME_LIBS_PATH
dnl guile-gnome is a set of wrappers, we need to check that the user
dnl has wrapped the gnome libraries foliot needs.  strictly speaking,
dnl the list below is 'too much' because some guile-gnome modules are
dnl always part of guile-gnome, but hey, no arm and nice to see all
dnl the checks.
GUILE_GNOME_MODULE_REQUIRED([gnome glib])
GUILE_GNOME_MODULE_REQUIRED([gnome gobject])
GUILE_GNOME_MODULE_REQUIRED([gnome gtk])
GUILE_GNOME_MODULE_REQUIRED([gnome gtk gdk-event])
GUILE_GNOME_MODULE_REQUIRED([gnome glade])

# Checks are in Grip_SQLite
PKG_CHECK_MODULES(SQLite3, sqlite3 >= 3.7)
PKG_CHECK_MODULES(libpcre, libpcre)

PKG_CHECK_MODULES([Grip], [grip-1.0 >= 0.2.0])
PKG_CHECK_MODULES([Grip_Gnome], [grip-gnome-1.0 >= 0.2.0])
PKG_CHECK_MODULES([Grip_SQLite], [grip-sqlite-1.0 >= 0.2.0])

###
### Guile-Lib
###

PKG_CHECK_MODULES(
  [GUILE_LIB],
  [guile-lib-1.0 >= 0.2.5],
  [HAVE_GUILE_LIB=yes],
  [HAVE_GUILE_LIB=no])


AC_CHECK_PROG(PDFLATEX, pdflatex, yes)
if test -z "$PDFLATEX"; then
  AC_MSG_ERROR([pdflatex required but not found])
fi

LATEX_DOCUMENT_CLASS_REQUIRED([extarticle])
LATEX_PACKAGE_REQUIRED([inputenc])
LATEX_PACKAGE_REQUIRED([fontenc])
LATEX_PACKAGE_REQUIRED([lmodern])
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
# LATEX_PACKAGE_AVAILABLE([HAS_IWONA], [iwona])
LATEX_PACKAGE_REQUIRED([iwona])

AC_CONFIG_FILES(
  [pre-inst-env],
  [chmod +x pre-inst-env])
AC_CONFIG_FILES(
  [foliot/foliot],
  [chmod +x foliot/foliot])
AC_CONFIG_FILES([
        Makefile
        doc/Makefile
        test-suite/Makefile
])
AC_OUTPUT

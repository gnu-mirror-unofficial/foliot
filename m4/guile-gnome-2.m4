## Autoconf macros for working with Guile-Gnome.
##
##   Copyright (C) 2015 Free Software Foundation, Inc.
##
## This library is free software; you can redistribute it and/or
## modify it under the terms of the GNU Lesser General Public License
## as published by the Free Software Foundation; either version 3 of
## the License, or (at your option) any later version.
##
## This library is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
## Lesser General Public License for more details.
##
## You should have received a copy of the GNU Lesser General Public
## License along with this library; if not, write to the Free Software
## Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
## 02110-1301 USA

# serial 1


###
### Index
###

## GUILE_GNOME_PROGS
## GUILE_GNOME_INST_DIR
## GUILE_GNOME_CHECK
## GUILE_GNOME_MODULE_CHECK
## GUILE_GNOME_MODULE_AVAILABLE
## GUILE_GNOME_MODULE_REQUIRED
## GUILE_GNOME_MODULE_EXPORTS
## GUILE_GNOME_MODULE_REQUIRED_EXPORT
## GUILE_GNOME_FLAGS

###
### Code
###

## NOTE: Comments preceding an AC_DEFUN (starting from "Usage:") are massaged
## into doc/ref/autoconf-macros.texi (see Makefile.am in that directory).


###
### GUILE_GNOME_PROGS
###   -- set path to the guile-gnome-2 program 
###

# Usage: GUILE_GNOME_PROGS([VERSION])
#
# This macro looks for the program @code{guile-gnome-2}, setting the
# variable @var{GUILE_GNOME} to its paths.  If @code{guile-gnome-2} is
# not found, he macro will signal an error.
#
# By default, this macro will search for the latest stable version of
# Guile-Gnome (e.g. 2.16). x.y or x.y.z versions can be specified. If
# an older version is found, the macro will signal an error.
#
# The effective version of the found @code{guile-gnome-2} is set to
# @var{GUILE_GNOME_EFFECTIVE_VERSION}.
#
# The variables are marked for substitution, as by @code{AC_SUBST}.
#
AC_DEFUN([GUILE_GNOME_PROGS],
 [AC_PATH_PROG(GUILE_GNOME, guile-gnome-2)
  _guile_gnome_required_version="m4_default([$1], [$GUILE_GNOME_EFFECTIVE_VERSION])"
  if test -z "$_guile_gnome_required_version"; then
    _guile_gnome_required_version=2.16
  fi
  if test "$GUILE_GNOME" = "" ; then
      AC_MSG_ERROR([guile-gnome-2 required but not found])
  fi
  AC_SUBST(GUILE_GNOME)

  _guile_gnome_effective_version=`$PKG_CONFIG --modversion guile-gnome-glib-2`
  _guile_gnome_major_version=`echo $_guile_gnome_effective_version | cut -d . -f 1`
  _guile_gnome_minor_version=`echo $_guile_gnome_effective_version | cut -d . -f 2`
  _guile_gnome_micro_version=`echo $_guile_gnome_effective_version | cut -d . -f 3`
  _guile_gnome_prog_version="$_guile_gnome_major_version.$_guile_gnome_minor_version.$_guile_gnome_micro_version"

  AC_MSG_CHECKING([for Guile-Gnome version >= $_guile_gnome_required_version])
  _major_version=`echo $_guile_gnome_required_version | cut -d . -f 1`
  _minor_version=`echo $_guile_gnome_required_version | cut -d . -f 2`
  _micro_version=`echo $_guile_gnome_required_version | cut -d . -f 3`
  if test "$_guile_gnome_major_version" -ge "$_major_version"; then
    if test "$_guile_gnome_minor_version" -ge "$_minor_version"; then
      if test -n "$_micro_version"; then
        if test "$_guile_gnome_micro_version" -lt "$_micro_version"; then
          AC_MSG_ERROR([Guile-Gnome $_guile_gnome_required_version required, but $_guile_gnome_prog_version found])
        fi
      fi
    elif test "$GUILE_GNOME_EFFECTIVE_VERSION" = "$_major_version.$_minor_version" -a -z "$_micro_version"; then
      # Allow prereleases that have the right effective version.
      true
    else
      as_fn_error $? "Guile-Gnome $_guile_gnome_required_version required, but $_guile_gnome_prog_version found" "$LINENO" 5
    fi
  else
    AC_MSG_ERROR([Guile-Gnome $_guile_gnome_required_version required, but $_guile_gnome_prog_version found])
  fi
  AC_MSG_RESULT([$_guile_gnome_prog_version])
 ])


###
### GUILE_GNOME_INST_DIR
###   -- find path of the Guile-Gnome directory
###

# Usage: GUILE_GNOME_INST_DIR
#
# This looks for Guile-Gnome's directory, usually something like
# PREFIX/share/guile-gnome-2, and sets var @var{GUILE_GNOME_DIR} to
# the path.  Note that the var name is different from the macro name.
#
# The variable is marked for substitution, as by @code{AC_SUBST}.
#
AC_DEFUN([GUILE_GNOME_INST_DIR],
 [AC_REQUIRE([GUILE_GNOME_PROGS])
  AC_MSG_CHECKING(for Guile-Gnome directory)
  GUILE_GNOME_DIR=`$GUILE_GNOME -c '(display (dirname (dirname (%search-load-path "gnome/glib.scm"))))'`
  AC_MSG_RESULT($GUILE_GNOME_DIR)
  if test "$GUILE_GNOME_DIR" = ""; then
     AC_MSG_FAILURE(directory not found)
  fi
  AC_SUBST(GUILE_GNOME_DIR)
 ])


###
### GUILE_GNOME_LIBS_PATH
###   -- find path to Guile-Gnome libraries
###

# Usage: GUILE_GNOME_LIBS_PATH
#
# This looks for Guile-Gnome libraries path, usually something like
# PREFIX/lib:PREFIX/lib/guile-gnome-2, and sets var
# @var{GUILE_GNOME_LTDL_LIBRARY_PATH} to the path.  Note that the var name is
# different from the macro name.
#
# The variable is marked for substitution, as by @code{AC_SUBST}.
#
AC_DEFUN([GUILE_GNOME_LIBS_PATH],
 [AC_REQUIRE([GUILE_GNOME_INST_DIR])
  _guile_gnome_libdir=`$PKG_CONFIG --variable=libdir guile-gnome-glib-2`
  _gg_dir_bname=`$GUILE_GNOME -c "(display (basename \"$GUILE_GNOME_DIR\"))"`
  GUILE_GNOME_LTDL_LIBRARY_PATH="$_guile_gnome_libdir:$_guile_gnome_libdir/$_gg_dir_bname"
  AC_SUBST([GUILE_GNOME_LTDL_LIBRARY_PATH])
 ])


###
### GUILE_GNOME_CHECK
###   -- evaluate Guile-Gnome Scheme code and capture the return value
###

#
# Usage: GUILE_GNOME_CHECK_RETVAL(var, check)
#
# @var{var} is a shell variable name to be set to the return value.
# @var{check} is a Guile Scheme expression, evaluated with
# "$GUILE_GNOME -c", and returning either 0 or non-#f to indicate the
# check passed.  Non-0 number or #f indicates failure.  Avoid using
# the character "#" since that confuses autoconf.
#
AC_DEFUN([GUILE_GNOME_CHECK],
 [AC_REQUIRE([GUILE_GNOME_PROGS])
  $GUILE_GNOME -c "$2" > /dev/null 2>&1
  $1=$?
 ])


###
### GUILE_GNOME_MODULE_CHECK
###   -- check feature of a Guile-Gnome Scheme module
###

# Usage: GUILE_GNOME_MODULE_CHECK(var, module, featuretest, description)
#
# @var{var} is a shell variable name to be set to "yes" or "no".
# @var{module} is a list of symbols, like: (gnome gobject).
# @var{featuretest} is an expression acceptable to GUILE_GNOME_CHECK, q.v.
# @var{description} is a present-tense verb phrase (passed to AC_MSG_CHECKING).
#
AC_DEFUN([GUILE_GNOME_MODULE_CHECK],
 [AC_MSG_CHECKING([if $2 $4])
  GUILE_GNOME_CHECK($1,(use-modules $2) (exit ((lambda () $3))))
  if test "$$1" = "0" ; then $1=yes ; else $1=no ; fi
  AC_MSG_RESULT($$1)
 ])


###
### GUILE_GNOME_MODULE_AVAILABLE
###   -- check availability of a Guile-Gnome Scheme module
###

# Usage: GUILE_GNOME_MODULE_AVAILABLE(var, module)
#
# @var{var} is a shell variable name to be set to "yes" or "no".
# @var{module} is a list of symbols, like: (gnome gobject).
#
AC_DEFUN([GUILE_GNOME_MODULE_AVAILABLE],
 [GUILE_GNOME_MODULE_CHECK($1,$2,0,is available)
 ])


###
### GUILE_GNOME_MODULE_REQUIRED
###   -- fail if a Guile-Gnome Scheme module is unavailable
###

# Usage: GUILE_GNOME_MODULE_REQUIRED(symlist)
#
# @var{symlist} is a list of symbols, WITHOUT surrounding parens,
# like: gnome gobject.
#
AC_DEFUN([GUILE_GNOME_MODULE_REQUIRED],
 [GUILE_GNOME_MODULE_AVAILABLE(ac_guile_gnome_module_required, ($1))
  if test "$ac_guile_gnome_module_required" = "no" ; then
    AC_MSG_ERROR([required guile-gnome module not found: ($1)])
  fi
 ])

###
### GUILE_GNOME_MODULE_EXPORTS
###   -- check if a module exports a variable
###

# Usage: GUILE_GNOME_MODULE_EXPORTS(var,module,modvar)
#
# @var{var} is a shell variable to be set to "yes" or "no".
# @var{module} is a list of symbols, like: (gnome gobject).
# @var{modvar} is the Guile-Gnome Scheme variable to check.
#
AC_DEFUN([GUILE_GNOME_MODULE_EXPORTS],
 [GUILE_GNOME_MODULE_CHECK($1,$2,$3,exports `$3')
 ])


###
### GUILE_GNOME_MODULE_REQUIRED_EXPORT
###   -- fail if a module doesn't export a variable
###

# Usage: GUILE_GNOME_MODULE_REQUIRED_EXPORT(module,modvar)
#
# @var{module} is a list of symbols, like: (gnome gobject).
# @var{modvar} is the Guile-Gnome Scheme variable to check.
#
AC_DEFUN([GUILE_GNOME_MODULE_REQUIRED_EXPORT],
 [GUILE_GNOME_MODULE_EXPORTS(guile_gnome_module_required_export,$1,$2)
  if test "$guile_gnome_module_required_export" = "no" ; then
    AC_MSG_ERROR([module $1 does not export $2; required])
  fi
 ])


# GUILE_GNOME_FLAGS -- set flags for compiling and linking with Guile_Gnome
#
# Usage: GUILE_GNOME_FLAGS
#
# This macro runs the @code{pkg-config} tool to find out how to compile
# and link programs against Guile-Gnome.  It sets four variables:
# @var{GUILE_GNOME_CFLAGS}, @var{GUILE_GNOME_LDFLAGS}, @var{GUILE_GNOME_LIBS}, and
# @var{GUILE_GNOME_LTLIBS}.
#
# @var{GUILE_GNOME_CFLAGS}: flags to pass to a C or C++ compiler to build code that
# uses Guile_Gnome header files.  This is almost always just one or more @code{-I}
# flags.
#
# @var{GUILE_GNOME_LDFLAGS}: flags to pass to the compiler to link a program
# against Guile-Gnome.  This includes @code{-lguile-gnome-@var{VERSION}} for the
# Guile-Gnome library itself, and may also include one or more @code{-L} flag
# to tell the compiler where to find the libraries.  But it does not
# include flags that influence the program's runtime search path for
# libraries, and will therefore lead to a program that fails to start,
# unless all necessary libraries are installed in a standard location
# such as @file{/usr/lib}.
#
# @var{GUILE_GNOME_LIBS} and @var{GUILE_GNOME_LTLIBS}: flags to pass to the compiler or to
# libtool, respectively, to link a program against Guile-Gnome.  It includes flags
# that augment the program's runtime search path for libraries, so that shared
# libraries will be found at the location where they were during linking, even
# in non-standard locations.  @var{GUILE_GNOME_LIBS} is to be used when linking the
# program directly with the compiler, whereas @var{GUILE_GNOME_LTLIBS} is to be used
# when linking the program is done through libtool.
#
# The variables are marked for substitution, as by @code{AC_SUBST}.
#
AC_DEFUN([GUILE_GNOME_FLAGS],
 [## AC_REQUIRE([GUILE_GNOME_PKG])
  PKG_CHECK_MODULES(GUILE_GNOME_GLIB, [guile-gnome-glib-2])

  dnl GUILE_GNOME_GLIB_CFLAGS and GUILE_GNOME_GLIB_LIBS are already
  dnl defined and AC_SUBST'd by PKG_CHECK_MODULES.  But
  dnl GUILE_GNOME_GLIB_LIBS to pkg-config is GUILE_GNOME_GLIB_LDFLAGS to
  dnl us.

  GUILE_GNOME_CFLAGS=$GUILE_GNOME_GLIB_CFLAGS
  GUILE_GNOME_LIBS=$GUILE_GNOME_GLIB_LIBS
  GUILE_GNOME_LDFLAGS=$GUILE_GNOME_LIBS

  AC_SUBST([GUILE_GNOME_CFLAGS])
  AC_SUBST([GUILE_GNOME_LIBS])
  AC_SUBST([GUILE_GNOME_LDFLAGS])
 ])

## guile-gnome-2.m4 ends here

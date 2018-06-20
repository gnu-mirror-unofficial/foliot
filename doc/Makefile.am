
####
#### Copyright (C) 2015 - 2018
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

# we use wildcard - non-POSIX variable name
# AUTOMAKE_OPTIONS = gnu

info_TEXINFOS = foliot.texi

DOC_CSS = \
	$(wildcard css/*)

foliot_TEXINFOS =	\
  version.texi		\
  variables.texi	\
  preface.texi     	\
  introduction.texi	\
  using-foliot.texi	\
  fdl.texi

nobase_doc_DATA = \
	$(DOC_CSS)

EXTRA_DIST = \
	$(DOC_CSS)

AM_MAKEINFOHTMLFLAGS = --css-ref=../css/manual.css

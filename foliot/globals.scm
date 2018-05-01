;; -*- mode: scheme; coding: utf-8 -*-

;;;;
;;;; Copyright (C) 2011 - 2016
;;;; Free Software Foundation, Inc.

;;;; This file is part of GNU Foliot.

;;;; GNU Foliot is free software: you can redistribute it and/or modify
;;;; it under the terms of the GNU General Public License as published
;;;; by the Free Software Foundation, either version 3 of the License,
;;;; or (at your option) any later version.

;;;; GNU Foliot is distributed in the hope that it will be useful, but
;;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;;; General Public License for more details.

;;;; You should have received a copy of the GNU General Public License
;;;; along with GNU Foliot.  If not, see <http://www.gnu.org/licenses/>.
;;;;

;;; Commentary:

;;; Code:


(define-module (foliot globals)
  #:use-module (grip module)
  #:use-module (grip utils))


(eval-when (expand load eval)
  (re-export-public-interface (grip utils))
  (let* ((foliotdir (dirname (search-path %load-path "foliot/foliot.scm")))
	 (pofdir (string-append foliotdir "/pof"))
	 (gtkrcdir (string-append foliotdir "/gtkrc"))
	 (iconsdir (string-append foliotdir "/icons"))
	 (gladedir (string-append foliotdir "/glade"))
	 (latexdir (string-append foliotdir "/latex"))
	 (printdir "/tmp"))
    (storage-set 'foliotdir foliotdir)
    (storage-set 'pofdir pofdir)
    (storage-set 'iconsdir iconsdir)
    (storage-set 'gladedir gladedir)
    (storage-set 'gladefile (string-append gladedir "/foliot.glade"))
    (storage-set 'gtkrcdir gtkrcdir)
    (storage-set 'gtkrcfile (string-append gtkrcdir "/gtkrc.foliot"))
    (storage-set 'latexdir latexdir)
    (storage-set 'printdir printdir)
    (storage-set 'imported-ids-delta 1000000)))


#!

(storage-get 'foliotdir)
(storage-get 'pofdir)
(storage-get 'gladedir)
(storage-get 'iconsdir)
(storage-get 'gladefile)
(storage-get 'gtkrcfile)
(storage-get 'latexdir)
(storage-get 'printdir)
(storage-get 'imported-ids-delta)

!#

;; -*- mode: scheme; coding: utf-8 -*-

;;;; Copyright (C) 2011 - 2015
;;;; Free Software Foundation, Inc.

;;;; This file is part of Kisê.

;;;; Kisê is free software: you can redistribute it and/or modify it
;;;; under the terms of the GNU General Public License as published by
;;;; the Free Software Foundation, either version 3 of the License, or
;;;; (at your option) any later version.

;;;; Kisê is distributed in the hope that it will be useful, but
;;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;;; General Public License for more details.

;;;; You should have received a copy of the GNU General Public License
;;;; along with Kisê.  If not, see <http://www.gnu.org/licenses/>.
;;;;

;;; Commentary:

;;; Code:


(define-module (kise globals)
  #:use-module (grip reexport)
  #:use-module (grip utils))


(eval-when (expand load eval)
  (re-export-public-interface (grip utils))
  (let* ((kisedir (dirname (search-path %load-path "kise/kise.scm")))
	 (pofdir (string-append kisedir "/pof"))
	 (gtkrcdir (string-append kisedir "/gtkrc"))
	 (iconsdir (string-append kisedir "/icons"))
	 (gladedir (string-append kisedir "/glade"))
	 (latexdir (string-append kisedir "/latex"))
	 (printdir "/tmp"))
    (storage-set 'kisedir kisedir)
    (storage-set 'pofdir pofdir)
    (storage-set 'iconsdir iconsdir)
    (storage-set 'gladedir gladedir)
    (storage-set 'gladefile (string-append gladedir "/kise.glade"))
    (storage-set 'gtkrcdir gtkrcdir)
    (storage-set 'gtkrcfile (string-append gtkrcdir "/gtkrc.kise"))
    (storage-set 'latexdir latexdir)
    (storage-set 'printdir printdir)
    (storage-set 'imported-ids-delta 1000000)
    ;; release: kisê to common git tag alist
    #;(storage-set 'tags '(("0.9.1" . "0.1")
			("0.9.2" . "0.2")
			("0.9.3" . "0.3")
			("0.9.4" . "0.4")))))


#!

(storage-get 'kisedir)
(storage-get 'pofdir)
(storage-get 'gladedir)
(storage-get 'iconsdir)
(storage-get 'gladefile)
(storage-get 'gtkrcfile)
(storage-get 'latexdir)
(storage-get 'printdir)
(storage-get 'imported-ids-delta)
#;(storage-get 'tags)

!#

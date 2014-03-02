;; -*- mode: scheme; coding: utf-8 -*-

;;;; Copyright (C) 2011, 2012, 2013
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
  ;; common
  :use-module (macros reexport)
  :use-module (system aglobs))


(eval-when (compile load eval)
  (re-export-public-interface (system aglobs))
  (let* ((kisedir (dirname (search-path %load-path "kise/kise.scm")))
	 (pofdir (string-append kisedir "/pof"))
	 (iconsdir (string-append kisedir "/icons"))
	 (gladedir (string-append kisedir "/glade")))
    (aglobs/set 'kisedir kisedir)
    (aglobs/set 'pofdir pofdir)
    (aglobs/set 'iconsdir iconsdir)
    (aglobs/set 'gladedir gladedir)
    (aglobs/set 'gladefile (string-append gladedir "/kise.glade"))
    (aglobs/set 'gtkrcfile (string-append kisedir "/gtkrc.kise"))
    (aglobs/set 'latexdir (string-append kisedir "/latex"))
    (aglobs/set 'printdir "/tmp")
    (aglobs/set 'imported-ids-delta 1000000)
    ;; release: kisê to common git tag alist
    (aglobs/set 'tags '(("0.9.1" . "0.1")
			("0.9.2" . "0.2")
			("0.9.3" . "0.3")
			("0.9.4" . "0.4")))))


#!
(use-modules (kise globals))
(reload-module (resolve-module '(kise globals)))

(aglobs/get 'kisedir)
(aglobs/get 'pofdir)
(aglobs/get 'gladedir)
(aglobs/get 'iconsdir)
(aglobs/get 'gladefile)
(aglobs/get 'gtkrcfile)
(aglobs/get 'latexdir)
(aglobs/get 'printdir)
(aglobs/get 'imported-ids-delta)
(aglobs/get 'tags)
!#

;; -*- mode: scheme; coding: utf-8 -*-

;;;; Copyright (C) 2011, 2012
;;;; Free Software Foundation, Inc.
;;;;
;;;; This library is free software; you can redistribute it and/or
;;;; modify it under the terms of the GNU Lesser General Public
;;;; License as published by the Free Software Foundation; either
;;;; version 3 of the License, or (at your option) any later version.
;;;; 
;;;; This library is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;;; Lesser General Public License for more details.
;;;; 
;;;; You should have received a copy of the GNU Lesser General Public
;;;; License along with this library; if not, write to the Free Software
;;;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
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
	 (gladedir (string-append kisedir "/glade")))
    (aglobs/set 'kisedir kisedir)
    (aglobs/set 'pofdir pofdir)
    (aglobs/set 'gladedir gladedir)
    (aglobs/set 'gladefile (string-append gladedir "/kise.glade"))
    (aglobs/set 'gtkrcfile (string-append kisedir "/gtkrc.kise"))
    (aglobs/set 'latexdir (string-append kisedir "/latex"))
    (aglobs/set 'printdir "/tmp")))


#!
(use-modules (kise globals))
(reload-module (resolve-module '(kise globals)))

(aglobs/get 'kisedir)
(aglobs/get 'pofdir)
(aglobs/get 'gladedir)
(aglobs/get 'gladefile)
(aglobs/get 'gtkrcfile)
(aglobs/get 'latexdir)
(aglobs/get 'printdir)
!#

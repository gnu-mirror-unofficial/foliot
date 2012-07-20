;; -*- mode: scheme; coding: utf-8 -*-

;;;; Copyright (C) 2011, 2012
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

(define-module (kise p-lvars)
  ;; guile
  :use-module (oop goops)

  ;; common
  :use-module (macros reexport)
  :use-module (macros when)
  :use-module (system dates)
  :use-module (system passwd)
  :use-module (system i18n)
  :use-module (system aglobs)
  :use-module (nbs all)
  :use-module (tex common)
  :use-module (db sqlite)

  ;; kise
  :use-module (kise config)
  :use-module (kise p-common)
  :use-module (kise p-dialog)

  :duplicates (merge-generics 
	       replace
	       warn-override-core
	       warn
	       last)

  :export (kp/write-local-variables))


(eval-when (compile load eval)
  (re-export-public-interface (system dates)
			      (system i18n)
			      (system aglobs))
  (textdomain "p-lvars")
  (bindtextdomain "p-lvars" (aglobs/get 'pofdir)))


;;;
;;; Globals and Non-API
;;;

(define (kp/write-local-variables-str)
  "
  \\def\\theprintingdate{~A}
  \\def\\draftreference{~A}
  \\def\\pagetext{~A}
  \\def\\tsreporttext{~A}
  \\def\\referencetext{~A}

  \\def\\klheader{~A}

  \\def\\klfooter{~A}
  \\def\\krfooter{~A}
")


;;;
;;; API
;;;

(define (kp/write-local-variables tex-files)
  (let* ((date (sys/date)) ;; will check but should use the locale
	 (lvars-tex-file-object (lvars tex-files))
	 (filename (full-filename lvars-tex-file-object))
	 (ostream (open-output-file filename))
	 (ulogo (kcfg/get 'ulogo)))
    (format ostream "~?" (kp/write-local-variables-str)
	    (list date ;; will check but should use the locale
		  (basename (pdf tex-files) ".pdf")
		  (_ "Page")
		  (_ "Time tracking Report")
		  (_ "reference")
		  ;; left header
		  (if ulogo
		      (format #f "\\includegraphics[height=26mm]{~A}" ulogo)
		      (format #f "\\klabel{~A}\\\\~~\\\\~~\\\\" (_ "Your logo here")))
		  ;; right header
		  ;;   this field is defined in draft-command
		  ;; footer
		  (format #f "~~~A ~A ~A ~A" 
			  (_ "printed on") date 
			  (_ "by") (sys/get 'uname))
		  "\\pagetext ~~ \\thepage/\\pageref{LastPage}~"))
    (close ostream)))


#!

(use-modules (kise p-lvars))
(reload-module (resolve-module '(kise p-lvars)))

(date/system-date "+%Y.%m.%d")

!#

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


(define-module (foliot p-lvars)
  #:use-module (oop goops)
  #:use-module (grip reexport)
  #:use-module (grip dates)
  #:use-module (grip passwd)
  #:use-module (grip i18n)
  #:use-module (grip utils)
  #:use-module (grip nbs)
  #:use-module (grip tex-utils)
  #:use-module (grip db sqlite)
  #:use-module (foliot config)
  #:use-module (foliot p-common)
  #:use-module (foliot p-dialog)

  #:export (fp/write-local-variables))


(eval-when (expand load eval)
  (re-export-public-interface (grip dates)
			      (grip i18n)
			      (grip utils))
  (textdomain "p-lvars")
  (bindtextdomain "p-lvars" (storage-get 'pofdir)))


;;;
;;; Globals and Non-API
;;;

(define (fp/write-local-variables-str)
  "
  \\def\\theprintingdate{~A}
  \\def\\draftreference{~A}
  \\def\\pagetext{~A}
  \\def\\tsreporttext{~A}
  \\def\\referencetext{~A}

  ~A

  \\def\\klfooter{~A}
  \\def\\krfooter{~A}
")


;;;
;;; API
;;;

(define (fp/write-logo-tex-code ostream ulogo)
  (if ulogo
      (format #f "\\def\\klheaderlogo{\\includegraphics[keepaspectratio=true,height=26mm,width=65mm]{~A}}" ulogo)
      ;; (format #f "\\def\\klheaderlogo{\\includegraphics[height=26mm]{~A}}" ulogo)
      (format #f "\\def\\klheader{\\klabel{~A}\\\\~~\\\\~~\\\\}" (_ "Your logo here"))))

(define (fp/write-local-variables tex-files)
  (let* ((date (sys/date)) ;; will check but should use the locale
	 (lvars-tex-file-object (lvars tex-files))
	 (filename (full-filename lvars-tex-file-object))
	 (ostream (open-output-file filename))
	 (ulogo (fcfg/get 'ulogo)))
    (format ostream "~?" (fp/write-local-variables-str)
	    (list date ;; will check but should use the locale
		  (basename (pdf tex-files) ".pdf")
		  (_ "Page")
		  (_ "Time Keeping Report")
		  (_ "reference")
		  ;; left header
		  (fp/write-logo-tex-code ostream ulogo)
		  ;; right header
		  ;;   this field is defined in draft-command
		  ;; footer
		  (format #f "~~~A ~A ~A ~A" 
			  (_ "printed on") date 
			  (_ "by") (sys/get 'uname))
		  "\\pagetext ~~ \\thepage/\\pageref{LastPage}~"))
    (close ostream)))


#!

(date/system-date "+%Y.%m.%d")

!#

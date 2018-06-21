;; -*- mode: scheme; coding: utf-8 -*-

;;;;
;;;; Copyright (C) 2011 - 2018
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


(define-module (foliot p-main)
  #:use-module (oop goops)
  #:use-module (gnome gobject)
  #:use-module (gnome gtk)
  #:use-module (grip module)
  #:use-module (grip passwd)
  #:use-module (grip date)
  #:use-module (grip i18n)
  #:use-module (grip utils)
  #:use-module (grip gnome filechooser)
  #:use-module (foliot globals)
  #:use-module (foliot p-dialog)
  #:use-module (foliot p-common)
  #:use-module (foliot p-lvars)
  #:use-module (foliot p-draft)
  #:use-module (foliot p-commercial)

  #:duplicates (merge-generics
		replace
		warn-override-core
		warn
		last)

  #:export (fp/print))


(eval-when (expand load eval)
  (textdomain "p-main")
  (bindtextdomain "p-main" (ref %foliot-store 'pofdir)))


(define (fp/print-1 fp/widget tl-widget pdf-filename)
  (let* ((reference (date/system-date "%Y%m%d"))
	 (tex-files (fp/common-filenames reference pdf-filename)))
    (fp/write-local-variables tex-files)
    (case (mode fp/widget)
      ((draft)
       (fp/print-draft fp/widget tl-widget tex-files))
      ((commercial)
       (fp/print-commercial fp/widget tl-widget tex-files)))))


;;;
;;; API
;;;

(define (fp/print fp/widget tl-widget)
  (let ((n-file (date/system-date "%Y.%m.%d")))
    (if (pdf fp/widget)
	(let* ((pdf-dir (format #f "~A/foliot" (sys/get 'udir)))
	       (proposed-name (case (mode fp/widget)
				((draft)
				 (format #f "foliot-~A-draft.pdf" n-file))
				((commercial)
				 (format #f "foliot-~A-commercial.pdf" n-file)))))
	  (unless (access? pdf-dir F_OK) (mkdir pdf-dir))
	  (let ((pdf-filename (prompt-for-filename (dialog fp/widget)
						   (_ "Save as ...")
						   'save
						   pdf-dir
						   proposed-name)))
	    (if pdf-filename (fp/print-1 fp/widget tl-widget pdf-filename))))
     (fp/print-1 fp/widget tl-widget #f))))

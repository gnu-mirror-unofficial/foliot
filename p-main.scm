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

(define-module (kise p-main)
  ;; guile/guile-gnome
  :use-module (oop goops)
  :use-module (gnome gobject)
  :use-module (gnome gtk)

  ;; common
  :use-module (macros reexport)
  :use-module (macros when)
  :use-module (system passwd)
  :use-module (system dates)
  :use-module (system i18n)
  :use-module (system aglobs)
  :use-module (gtk filechooser)

  ;; kise
  :use-module (kise p-dialog)
  :use-module (kise p-common)
  :use-module (kise p-lvars)
  :use-module (kise p-draft)
  :use-module (kise p-commercial)

  :duplicates (merge-generics 
	       replace
	       warn-override-core
	       warn
	       last)

  :export (kp/print))


(eval-when (compile load eval)
  (textdomain "p-main")
  (bindtextdomain "p-main" (aglobs/get 'pofdir)))


(define (kp/print-1 kp/widget tl-widget pdf-filename)
  (let* ((reference (date/system-date "%Y%m%d"))
	 (tex-files (kp/common-filenames reference pdf-filename)))
    (kp/write-local-variables tex-files)
    (case (mode kp/widget)
      ((draft)
       (kp/print-draft kp/widget tl-widget tex-files))
      ((commercial)
       (kp/print-commercial kp/widget tl-widget tex-files))
      )))


;;;
;;; API
;;;

(define (kp/print kp/widget tl-widget)
  (let ((n-file (date/system-date "%Y.%m.%d")))
    (if (pdf kp/widget)
	(let* ((pdf-dir (format #f "~A/kise" (sys/get 'udir)))
	       (proposed-name (case (mode kp/widget)
				((draft)
				 (format #f "kise-~A-draft.pdf" n-file))
				((commercial)
				 (format #f "kise-~A-commercial.pdf" n-file)))))
	  (unless (access? pdf-dir F_OK) (mkdir pdf-dir))
	  (let ((pdf-filename (prompt-for-file-name (dialog kp/widget)
						    (_ "Save as ...")
						    'save
						    pdf-dir
						    proposed-name
						    )))
	    (unless (or (eq? pdf-filename 'cancel)
			(eq? pdf-filename 'delete))
	      (kp/print-1 kp/widget tl-widget pdf-filename))))
     (kp/print-1 kp/widget tl-widget #f))))


#!

(use-modules (kise p-main))
(reload-module (resolve-module '(kise p-main)))

!#

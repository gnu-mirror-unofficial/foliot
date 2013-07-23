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

(define-module (kise import)
  ;; guile/guile-gnome
  :use-module (oop goops)

  ;; common
  :use-module (macros reexport)
  :use-module (macros do)
  :use-module (gtk all)
  :use-module (system i18n)

  :use-module (kise tl-widget)
  :use-module (kise i-dialog)
  :use-module (kise db)

  :export (ki/select-gui
	   #;ki/import))


(eval-when (compile load eval)
  (re-export-public-interface (oop goops)
			      (kise i-dialog)))


(define (ki/reimport tl-widget ki-widget)
  ;; (dimfi "ktlw/import callback called")
  (let ((model (tv-model ki-widget))
	(selection (tv-sel ki-widget))
	(prev-gui-cb? (gui-callback? tl-widget))
	(prev-active-filter (active-filter tl-widget)))
    (dotimes (i (gtk-tree-model-iter-n-children model #f))
      (when (iter-is-selected selection (get-iter model i))
	(let* ((iter (get-iter model i))
	       (filename (kiiter/get 'filename model iter)))
	  (db-kise/import filename)
	  (kiiter/set 'date model iter (date/system-date)))))
    (set! (gui-callback? tl-widget) #f)
    (set! (active-filter tl-widget) #t)
    (ktlw/filter-clear tl-widget 'fillcombos)
    (set! (gui-callback? tl-widget) prev-gui-cb?)
    (set! (active-filter tl-widget) prev-active-filter)))


;;;
;;; API
;;;

(define (ki/select-gui tl-widget)
  (let* ((parent (dialog tl-widget))
	 (g-file (glade-file tl-widget))
	 (ki-widget (ki/make-dialog parent g-file))
	 (widget (dialog ki-widget)))

    (connect (reimport-bt ki-widget)
	     'clicked
	     (lambda (button)
	       (ki/reimport tl-widget ki-widget)))

    (show widget)
    (catch 'exit
	   (lambda ()
	     (let ((response (genum->symbol (make <gtk-response-type> :value (run widget)))))
	       ;; (dimfi "ki/select-gui" response)
	       (hide widget)
	       (case response
		 ((close) ;; -7 in glade files
		  (throw 'exit 'close))
		 ((delete cancel)
		  (throw 'exit 'delete)))))
	   (lambda (key value)
	     value))))


#!

(use-modules (kise import))
(reload-module (resolve-module '(kise import)))

!#

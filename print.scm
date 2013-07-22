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

(define-module (kise print)
  ;; guile/guile-gnome
  :use-module (oop goops)
  :use-module (gnome gobject)
  :use-module (gnome gtk)

  ;; common
  ;; :use-module (system reexport)
  :use-module (gtk all)

  :use-module (kise globals)
  :use-module (kise tl-widget)
  :use-module (kise p-dialog)
  :use-module (kise p-main)

  :export (kp/select-gui))


#!
(eval-when (compile load eval)
  (re-export-public-interface (oop goops)
			      (gnome gobject)
			      (gnome gtk)
			      (gtk all)
			      (kise globals)
			      (kise tl-widget)
			      (kise p-dialog)
			      (kise p-main)))
!#


;;;
;;; API
;;;

(define (kp/select-gui tl-widget)
  (let* ((parent (dialog tl-widget))
	 (g-file (glade-file tl-widget))
	 (kp/widget (kp/make-dialog parent g-file))
	 (widget (dialog kp/widget)))
    ;; (format #t "Printing Widget: ~S~%Parent: ~S~%Printing Dialog: ~S~%" kp/widget parent widget)
    (show widget)
    (set-modal widget #f)
    (catch 'exit
	   (lambda ()
	     (let ((response (genum->symbol (make <gtk-response-type> :value (run widget)))))
	       (hide widget)
	       (case response
		 ((ok)
		  (kp/print kp/widget tl-widget)
		  (throw 'exit 'ok))
		 ((delete cancel)
		  (throw 'exit 'cancel)))))
	   (lambda (key value)
	     value))))


#!

(use-modules (kise print))
(reload-module (resolve-module '(kise print)))

(kp/select-gui tl-widget)


;;;
;;; Other tests
;;;

guile-gnome-2
(use-modules (oop goops)
	     (gnome gobject)
	     (gnome gtk)
	     (gnome glade))
dialog	;; in guile-gnome?


;;;
;;; 1.
;;;

(use-modules (kise print))
,m (kise print)
(define tl-widget (make <kise/tl-widget>))
(define p-widget (kp/make-dialog (dialog tl-widget) (aglobs/get 'gladefile)))

(dialog tl-widget)
(dialog p-widget)


;;;
;;; 2.
;;;

(use-modules (kise print))
,m (kise print)
(define p-widget (kp/make-dialog #f (aglobs/get 'gladefile)))

(dialog p-widget)


;;;
;;; 3.
;;;

(use-modules (kise print))
,m (kise print)
(define tl-widget (make-tl-widget))
(define p-widget (kp/make-dialog (dialog tl-widget) (aglobs/get 'gladefile)))

(dialog p-widget)
(dialog tl-widget)

!#

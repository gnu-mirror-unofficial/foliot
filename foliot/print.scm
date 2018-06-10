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


(define-module (foliot print)
  #:use-module (oop goops)
  #:use-module (gnome gobject)
  #:use-module (gnome gtk)
  ;; #:use-module (grip module)
  #:use-module (grip gnome)
  #:use-module (foliot globals)
  #:use-module (foliot tl-widget)
  #:use-module (foliot p-dialog)
  #:use-module (foliot p-main)

  #:duplicates (merge-generics
		replace
		warn-override-core
		warn
		last)

  #:export (fp/select-gui))


#;(eval-when (expand load eval)
  (re-export-public-interface (oop goops)
			      (gnome gobject)
			      (gnome gtk)
			      (gnome all)
			      (foliot globals)
			      (foliot tl-widget)
			      (foliot p-dialog)
			      (foliot p-main)))


;;;
;;; API
;;;

(define (fp/select-gui tl-widget)
  (let* ((parent (dialog tl-widget))
	 (g-file (glade-file tl-widget))
	 (fp/widget (fp/make-dialog parent g-file))
	 (widget (dialog fp/widget)))
    ;; (format #t "Printing Widget: ~S~%Parent: ~S~%Printing Dialog: ~S~%" fp/widget parent widget)
    (show widget)
    (set-modal widget #f)
    (catch 'exit
	   (lambda ()
	     (let ((response (genum->symbol (make <gtk-response-type> #:value (run widget)))))
	       (hide widget)
	       (case response
		 ((ok)
		  (fp/print fp/widget tl-widget)
		  (throw 'exit 'ok))
		 ((delete cancel)
		  (throw 'exit 'cancel)))))
	   (lambda (key value)
	     value))))


#!

(fp/select-gui tl-widget)


;;;
;;; Other tests
;;;

dialog	;; in guile-gnome?


;;;
;;; 1.
;;;

(define tl-widget (make <foliot/tl-widget>))
(define p-widget (fp/make-dialog (dialog tl-widget) (ref %foliot-store 'gladefile)))

(dialog tl-widget)
(dialog p-widget)


;;;
;;; 2.
;;;

(define p-widget (fp/make-dialog #f (ref %foliot-store 'gladefile)))

(dialog p-widget)


;;;
;;; 3.
;;;

(define tl-widget (make-tl-widget))
(define p-widget (fp/make-dialog (dialog tl-widget) (ref %foliot-store 'gladefile)))

(dialog p-widget)
(dialog tl-widget)

!#

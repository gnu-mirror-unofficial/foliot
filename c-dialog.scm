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

(define-module (kise c-dialog)
  ;; guile/guile-gnome
  :use-module (ice-9 format)
  :use-module (oop goops)
  :use-module (gnome gobject)
  :use-module (gnome glade)
  :use-module (gnome gtk)

  ;; common
  :use-module (system i18n)
  :use-module (gtk all)

  ;; kise
  :use-module (kise colours) ;; <- later use aglobs/set/get and delete this file

  :export (kc/close-dialog
	   kc/make-dialog
	   <kc/widget>
	   dialog
	   mode
	   reuse-db-cb
	   ok-bt
	   cancel-bt))


(define *kc-widget* #f)

(define (kc/close-dialog kc-dialog)
  (destroy kc-dialog)
  (set! *kc-widget* #f))

#!
(set-modal kc-dialog #f)
(hide kc-dialog)
!#


;;;
;;; Goops and API
;;;

(define-class <kc/widget> ()
  (xml-code :accessor xml-code :init-keyword :xml-code :init-value #f)
  (dialog :accessor dialog :init-keyword :dialog :init-value #f)
  (select-rb :accessor select-rb :init-keyword :select-rb :init-value #f)
  (create-rb :accessor create-rb :init-keyword :create-rb :init-value #f)
  (mode :accessor mode :init-keyword :mode :init-value #f)
  (reuse-db-cb :accessor reuse-db-cb :init-keyword :reuse-db-cb :init-value #f)
  (ok-bt :accessor ok-bt :init-keyword :ok-bt :init-value #f)
  (cancel-bt :accessor cancel-bt :init-keyword :cancel-bt :init-value #f))

(define (kc/make-dialog parent glade-f)
  (if *kc-widget*
      *kc-widget*
      (let* ((xmlc (glade-xml-new glade-f #f "kc/dialog"))
	     (widget (get-widget xmlc "kc/dialog"))
	     (kc-widget (make <kc/widget>
			  :xml-code xmlc
			  :dialog widget
			  :select-rb (get-widget xmlc "kc/select_rb")
			  :create-rb (get-widget xmlc "kc/create_rb")
			  :reuse-db-cb (get-widget xmlc "kc/reuse_db_cb")
			  :ok-bt (get-widget xmlc "kc/ok_bt")
			  :cancel-bt (get-widget xmlc "kc/cancel_bt"))))
	(modify-bg (get-widget xmlc "kc/eventbox") 'normal *dialog-title-eb-bg*)
	(when parent (set-transient-for widget parent))
	(kc/translate kc-widget)

	(connect (dialog kc-widget)
		 'destroy-event
		 (lambda (widget event)
		   (set! *kc-widget* #f)
		   #f))
	(connect (dialog kc-widget)
		 'delete-event
		 (lambda (widget event)
		   (set! *kc-widget* #f)
		   #f))
	(connect (select-rb kc-widget)
		 'toggled
		 (lambda (widget)
		   (set-action (dialog kc-widget) 'open)
		   (set! (mode kc-widget) 'open)))
	(connect (create-rb kc-widget)
		 'toggled
		 (lambda (widget)
		   (let ((current-folder (get-current-folder (dialog kc-widget))))
		     (set-action (dialog kc-widget) 'save)
		     ;; necessary: if not it bugs
		     (set-current-folder (dialog kc-widget) current-folder)
		     (set! (mode kc-widget) 'create))))
	(connect (reuse-db-cb kc-widget)
		 'toggled
		 (lambda (widget)
		   (let ((value (get-active widget)))
		     ;; (format #t "use this db for next sessions is ~A~%" (if value "ON" "OFF"))		
		     #f)))

	(set-active (select-rb kc-widget) #t)
	(emit (select-rb kc-widget) 'toggled)

	(set! *kc-widget* kc-widget)
	kc-widget)))


;;;
;;; i18n - localisation
;;;

(define (kc/translate widget)
  ;; we will :)
  #f)


#!

(use-modules (kise c-dialog))
(reload-module (resolve-module '(kise c-dialog)))

!#
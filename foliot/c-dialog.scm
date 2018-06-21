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


(define-module (foliot c-dialog)
  #:use-module (ice-9 format)
  #:use-module (oop goops)
  #:use-module (gnome gobject)
  #:use-module (gnome glade)
  #:use-module (gnome gtk)
  #:use-module (grip module)
  #:use-module (grip i18n)
  #:use-module (grip gnome)
  #:use-module (foliot colours) ;; <- later use storage-set/get and delete this file

  #:duplicates (merge-generics
		replace
		warn-override-core
		warn
		last)

  #:export (fc/close-dialog
	    fc/make-dialog
	    <fc/widget>))

(g-export dialog
          mode
          reuse-db-cb
          ok-bt
          cancel-bt)


(define *fc-widget* #f)

(define (fc/close-dialog fc-dialog)
  (destroy fc-dialog)
  (set! *fc-widget* #f))

#!
(set-modal fc-dialog #f)
(hide fc-dialog)
!#


;;;
;;; Goops and API
;;;

(define-class <fc/widget> ()
  (xml-code #:accessor xml-code #:init-keyword #:xml-code #:init-value #f)
  (dialog #:accessor dialog #:init-keyword #:dialog #:init-value #f)
  (select-rb #:accessor select-rb #:init-keyword #:select-rb #:init-value #f)
  (create-rb #:accessor create-rb #:init-keyword #:create-rb #:init-value #f)
  (mode #:accessor mode #:init-keyword #:mode #:init-value #f)
  (reuse-db-cb #:accessor reuse-db-cb #:init-keyword #:reuse-db-cb #:init-value #f)
  (ok-bt #:accessor ok-bt #:init-keyword #:ok-bt #:init-value #f)
  (cancel-bt #:accessor cancel-bt #:init-keyword #:cancel-bt #:init-value #f))

(define (fc/make-dialog parent glade-f)
  (if *fc-widget*
      *fc-widget*
      (let* ((xmlc (glade-xml-new glade-f #f "fc/dialog"))
	     (widget (get-widget xmlc "fc/dialog"))
	     (fc-widget (make <fc/widget>
			  #:xml-code xmlc
			  #:dialog widget
			  #:select-rb (get-widget xmlc "fc/select_rb")
			  #:create-rb (get-widget xmlc "fc/create_rb")
			  #:reuse-db-cb (get-widget xmlc "fc/reuse_db_cb")
			  #:ok-bt (get-widget xmlc "fc/ok_bt")
			  #:cancel-bt (get-widget xmlc "fc/cancel_bt"))))
	(modify-bg (get-widget xmlc "fc/eventbox") 'normal *dialog-title-eb-bg*)
	(when parent (set-transient-for widget parent))
	(fc/translate fc-widget)

	(connect (dialog fc-widget)
		 'destroy-event
		 (lambda (widget event)
		   (set! *fc-widget* #f)
		   #f))
	(connect (dialog fc-widget)
		 'delete-event
		 (lambda (widget event)
		   (set! *fc-widget* #f)
		   #f))
	(connect (select-rb fc-widget)
		 'toggled
		 (lambda (widget)
		   (set-action (dialog fc-widget) 'open)
		   (set! (mode fc-widget) 'open)))
	(connect (create-rb fc-widget)
		 'toggled
		 (lambda (widget)
		   (let ((current-folder (get-current-folder (dialog fc-widget))))
		     (set-action (dialog fc-widget) 'save)
		     ;; necessary#: if not it bugs
		     (set-current-folder (dialog fc-widget) current-folder)
		     (set! (mode fc-widget) 'create))))
	(connect (reuse-db-cb fc-widget)
		 'toggled
		 (lambda (widget)
		   (let ((value (get-active widget)))
		     ;; (format #t "use this db for next sessions is ~A~%"
		     ;;         (if value "ON" "OFF"))		
		     #f)))

	(set-active (select-rb fc-widget) #t)
	(emit (select-rb fc-widget) 'toggled)

	(set! *fc-widget* fc-widget)
	fc-widget)))


;;;
;;; i18n - localisation
;;;

(define (fc/translate widget)
  ;; we will :)
  #f)

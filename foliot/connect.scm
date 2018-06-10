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


(define-module (foliot connect)
  #:use-module (oop goops)
  #:use-module (gnome gobject)
  #:use-module (gnome gtk)
  ;; #:use-module (grip module)
  #:use-module (grip gnome)
  #:use-module (grip i18n)
  #:use-module (foliot globals)
  #:use-module (foliot db)
  #:use-module (foliot tl-widget)
  #:use-module (foliot config)
  #:use-module (foliot c-dialog)

  #:duplicates (merge-generics
		replace
		warn-override-core
		warn
		last)

  #:export (fc/select-gui))


#!
(eval-when (expand load eval)
  (re-export-public-interface (oop goops)
			      (gnome gobject)
			      (gnome gtk)

			      (grip gnome)
			      (grip i18n)

			      (foliot db)
			      (foliot tl-widget)
			      (foliot config)
			      (foliot c-dialog)))
!#


;;;
;;; Globals
;;;


;;;
;;; API
;;;

(define *db-already-in-use-msg*
  (_ "is the active database."))

(define (fc/connect-cant-connect-str)
  (_ "I can not open ~A: or it is not a GNU Foliot database file, or you don't have writing permissions over it."))

#!
Some problem occured while trying to open ~A. It could be that you
don't have writing permissions over it, or that it is not a GNU Foliot
database file. Please check all of the above and start again or
create/connect to another GNU Foliot database.
!#

(define (fc/connect-cant-create-str)
  (_ "You don't have 'writing permissions' in this directory: ~A. Please check your permissions and try again or make another directory selection."))

(define (fc/connect-create-exists-str)
  (_ "A file named ~A already exists. Please select another name or another directory."))

(define (fc/connect tl-widget fc-widget)
  (let* ((fc-dialog (dialog fc-widget))
	 (filename (get-filename fc-dialog))
	 (reuse-db? (get-active (reuse-db-cb fc-widget)))
	 (active-db-file (and (db-con) (fcfg/get 'db-file))))
    (if (and active-db-file
	     (string=? filename active-db-file))
	(md1b/select-gui (dialog fc-widget)
			 (_ "Information")
			 (_ "Db already in use:")
			 (format #f "~A: ~A" filename *db-already-in-use-msg*)
			 (lambda () 'nothing)
			 'dialog-info)
	(case (mode fc-widget)
	  ((open)
	   ;; the user could select a 'wrong file'. all checks must be done but 'exists
	   (receive (checks-result db)
	       (ftlw/open-db-checks filename)
	     (case checks-result
	       ((wrong-perm not-an-sqlite-file)
		(md1b/select-gui (dialog tl-widget)
				 (_ "Warning!")
				 (_ "DB connection problem:")
				 (format #f "~?" (fc/connect-cant-connect-str) (list filename))
				 (lambda () 'nothing)
				 'dialog-warning))
	       ((opened opened-partial-schema opened-no-schema)
		(ftlw/open-db tl-widget filename 'from-gui 'open reuse-db? checks-result db)
		(fc/close-dialog fc-dialog)))))
	  ((create)
	   ;; for some very obscure reasons, when in 'create' mode, fc/connect is called 2x ... see
	   ;; foliot-bugs for details.  (format #t "modal?: ~S // New db for foliot in ~A~%" (get-modal
	   ;; fc-dialog) filename)
	   (when (get-modal fc-dialog)
	     (let ((checks-result (ftlw/create-db-checks filename)))
	       (case checks-result
		 ((exists)
		  (md1b/select-gui (dialog tl-widget)
				   (_ "Warning!")
				   (_ "DB creation problem:")
				   (format #f "~?" (fc/connect-create-exists-str) (list (basename filename)))
				   (lambda () 'nothing)
				   'dialog-warning))
		 ((wrong-perm)
		  (md1b/select-gui (dialog tl-widget)
				   (_ "Warning!")
				   (_ "DB creation problem:")
				   (format #f "~?" (fc/connect-cant-create-str) (list (dirname filename)))
				   (lambda () 'nothing)
				   'dialog-warning))
		 ((ok opened)
		  (ftlw/open-db tl-widget filename 'from-gui 'create reuse-db? checks-result #f)
		  (fc/close-dialog fc-dialog))))))))))

(define (fc/select-gui tl-widget)
  (let* ((parent (dialog tl-widget))
	 (g-file (glade-file tl-widget))
	 (db-file (fcfg/get 'db-file))
	 (reuse-db? (or (not db-file) (fcfg/get 'open-at-startup)))
	 (fc-widget (fc/make-dialog parent g-file))
	 (fc-dialog (dialog fc-widget)))
    ;; (format #t "Connecting Widget: ~S~%Parent: ~S~%Connecting Dialog: ~S~%"
    ;;         fc-widget parent fc-dialog)
    (if reuse-db?
	(set-active (reuse-db-cb fc-widget) #t))
    (if db-file
	;; (set-current-folder fc-dialog (dirname db-file))
	(select-filename fc-dialog db-file)
	(set-current-folder fc-dialog (sys/get 'udir)))
    ;; this is not allowed in 'open mode, which is the default (if fname
    ;; (set-current-name fc-dialog fname))
    (connect (ok-bt fc-widget)
	     'clicked
	     (lambda (button)
	       (fc/connect tl-widget fc-widget)))
    (connect (cancel-bt fc-widget)
	     'clicked
	     (lambda (button)
	       (fc/close-dialog fc-dialog)))
    (set-modal fc-dialog #t)
    (show fc-dialog)))


#!

(define tl-widget (make <foliot/tl-widget>
		    #:glade-file (ref %foliot-store 'gladefile)))
(fc/select-gui tl-widget "/usr/alto/db" "sqlite.alto.db")

(define c-widget (fc/make-dialog (dialog tl-widget) (ref %foliot-store 'gladefile)))
(dialog tl-widget)
(dialog c-widget)

!#

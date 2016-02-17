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


(define-module (foliot connect)
  #:use-module (oop goops)
  #:use-module (gnome gobject)
  #:use-module (gnome gtk)
  ;; #:use-module (grip reexport)
  #:use-module (grip gnome)
  #:use-module (grip i18n)
  #:use-module (foliot db)
  #:use-module (foliot tl-widget)
  #:use-module (foliot config)
  #:use-module (foliot c-dialog)

  #:export (kc/select-gui))


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

(define (kc/connect-cant-connect-str)
  (_ "I can not open ~A: or it is not a GNU Foliot database file, or you don't have writing permissions over it."))

#!
Some problem occured while trying to open ~A. It could be that you
don't have writing permissions over it, or that it is not a GNU Foliot
database file. Please check all of the above and start again or
create/connect to another GNU Foliot database.
!#

(define (kc/connect-cant-create-str)
  (_ "You don't have 'writing permissions' in this directory: ~A. Please check your permissions and try again or make another directory selection."))

(define (kc/connect-create-exists-str)
  (_ "A file named ~A already exists. Please select another name or another directory."))

(define (kc/connect tl-widget kc-widget)
  (let* ((kc-dialog (dialog kc-widget))
	 (filename (get-filename kc-dialog))
	 (reuse-db? (get-active (reuse-db-cb kc-widget)))
	 (active-db-file (and (db-con) (kcfg/get 'db-file))))
    (if (and active-db-file
	     (string=? filename active-db-file))
	(md1b/select-gui (dialog kc-widget)
			 (_ "Information")
			 (_ "Db already in use:")
			 (format #f "~A: ~A" filename *db-already-in-use-msg*)
			 (lambda () 'nothing)
			 'dialog-info)
	(case (mode kc-widget)
	  ((open)
	   ;; the user could select a 'wrong file'. all checks must be done but 'exists
	   (receive (checks-result db)
	       (ktlw/open-db-checks filename)
	     (case checks-result
	       ((wrong-perm not-an-sqlite-file)
		(md1b/select-gui (dialog tl-widget)
				 (_ "Warning!")
				 (_ "DB connection problem:")
				 (format #f "~?" (kc/connect-cant-connect-str) (list filename))
				 (lambda () 'nothing)
				 'dialog-warning))
	       ((opened opened-partial-schema opened-no-schema)
		(ktlw/open-db tl-widget filename 'from-gui 'open reuse-db? checks-result db)
		(kc/close-dialog kc-dialog)))))
	  ((create)
	   ;; for some very obscure reasons, when in 'create' mode, kc/connect is called 2x ... see
	   ;; foliot-bugs for details.  (format #t "modal?: ~S // New db for foliot in ~A~%" (get-modal
	   ;; kc-dialog) filename)
	   (when (get-modal kc-dialog)
	     (let ((checks-result (ktlw/create-db-checks filename)))
	       (case checks-result
		 ((exists)
		  (md1b/select-gui (dialog tl-widget)
				   (_ "Warning!")
				   (_ "DB creation problem:")
				   (format #f "~?" (kc/connect-create-exists-str) (list (basename filename)))
				   (lambda () 'nothing)
				   'dialog-warning))
		 ((wrong-perm)
		  (md1b/select-gui (dialog tl-widget)
				   (_ "Warning!")
				   (_ "DB creation problem:")
				   (format #f "~?" (kc/connect-cant-create-str) (list (dirname filename)))
				   (lambda () 'nothing)
				   'dialog-warning))
		 ((ok opened)
		  (ktlw/open-db tl-widget filename 'from-gui 'create reuse-db? checks-result #f)
		  (kc/close-dialog kc-dialog))))))))))

(define (kc/select-gui tl-widget)
  (let* ((parent (dialog tl-widget))
	 (g-file (glade-file tl-widget))
	 (db-file (kcfg/get 'db-file))
	 (reuse-db? (or (not db-file) (kcfg/get 'open-at-startup)))
	 (kc-widget (kc/make-dialog parent g-file))
	 (kc-dialog (dialog kc-widget)))
    ;; (format #t "Connecting Widget: ~S~%Parent: ~S~%Connecting Dialog: ~S~%"
    ;;         kc-widget parent kc-dialog)
    (if reuse-db?
	(set-active (reuse-db-cb kc-widget) #t))
    (if db-file
	;; (set-current-folder kc-dialog (dirname db-file))
	(select-filename kc-dialog db-file)
	(set-current-folder kc-dialog (sys/get 'udir)))
    ;; this is not allowed in 'open mode, which is the default (if fname
    ;; (set-current-name kc-dialog fname))
    (connect (ok-bt kc-widget)
	     'clicked
	     (lambda (button)
	       (kc/connect tl-widget kc-widget)))
    (connect (cancel-bt kc-widget)
	     'clicked
	     (lambda (button)
	       (kc/close-dialog kc-dialog)))
    (set-modal kc-dialog #t)
    (show kc-dialog)))


#!

(define tl-widget (make <foliot/tl-widget>
		    #:glade-file (storage-get 'gladefile)))
(kc/select-gui tl-widget "/usr/alto/db" "sqlite.alto.db")

(define c-widget (kc/make-dialog (dialog tl-widget) (storage-get 'gladefile)))
(dialog tl-widget)
(dialog c-widget)

!#

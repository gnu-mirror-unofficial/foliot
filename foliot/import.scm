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


(define-module (foliot import)
  #:use-module (oop goops)
  #:use-module (grip module)
  #:use-module (grip iter)
  #:use-module (grip i18n)
  #:use-module (grip gnome)
  #:use-module (foliot config)
  #:use-module (foliot tl-widget)
  #:use-module (foliot i-dialog)
  #:use-module (foliot db)

  #:duplicates (merge-generics
		replace
		warn-override-core
		warn
		last)

  #:export (fi/select-gui))


(eval-when (expand load eval)
  (re-export-public-interface (oop goops)
			      (foliot i-dialog)))


(define *no-available-colour-set-msg*
  (_ "All available colour-set are already in use. For this import, we will reuse the '~A' [id '~A'] colour-set."))

(define *importing-the-active-db-is-not-allowed-msg*
  (_ "importing the active db is not allowed"))

(define *no-read-perm-msg*
  (_ "no read permission upon this file"))

(define *not-an-sqlite-file-msg*
  (_ "not an sqlite file"))

(define *not-foliot-table-msg*
  (_ "no foliot table"))

(define (fi/add tl-widget fi-widget)
  (let ((model (tv-model fi-widget))
	(selection (tv-sel fi-widget))
	(prev-gui-cb? (gui-callback? tl-widget))
	(prev-active-filter (active-filter tl-widget))
	(import-filename (prompt-for-filename (dialog fi-widget)
					      (_ "Import database selection")
					      'open
					      (dirname (fcfg/get 'db-file))
					      #f)))
    (if import-filename
	(let ((pre-checks-failed? (cond ((string=? import-filename (fcfg/get 'db-file))
					 *importing-the-active-db-is-not-allowed-msg*)
					((not (access? import-filename R_OK))
					 *no-read-perm-msg*)
					((not (sqlite/sqlite-db-file? import-filename))
					 *not-an-sqlite-file-msg*)
					(else #f))))
	  (if pre-checks-failed?
	      (begin
		(md1b/select-gui (dialog fi-widget)
				 (_ "Warning!")
				 (_ "Import db pre checks failed:")
				 (format #f "~A: ~A" import-filename pre-checks-failed?)
				 (lambda () 'nothing)
				 'dialog-warning)
		#f)
	      (let* ((idb-con (db-con/open import-filename #f))
		     (foliot? (sqlite/table-exists? idb-con "foliot")))
		(if (not foliot?)
		    (begin
		      (md1b/select-gui (dialog fi-widget)
				       (_ "Warning!")
				       (_ "Import db pre checks failed:")
				       (format #f "~A: ~A" import-filename *not-foliot-table-msg*)
				       (lambda () 'nothing)
				       'dialog-warning)
		      #f)
		    (let* ((id (db-idb/get-next-id))
			   (idb-cs (modulo id (length %palette)))
			   (idb-id (db-foliot/import import-filename idb-cs id idb-con)))
		      (if (>= id (length %palette))
			  (md1b/select-gui (dialog fi-widget)
					   (_ "Warning!")
					   (_ "Colour set:")
					   (format #f "~?" *no-available-colour-set-msg*
						   (list (color-set-name idb-cs) idb-cs))
					   (lambda () 'nothing)
					   'dialog-warning))
		      (set! (gui-callback? tl-widget) #f)
		      (unless prev-active-filter (set! (active-filter tl-widget) #t))
		      (ftlw/filter-clear tl-widget 'fillcombos)
		      (set! (gui-callback? tl-widget) prev-gui-cb?)
		      (set! (active-filter tl-widget) prev-active-filter)
		      (fi/fill-treeview fi-widget (db-idb/select-all 'display))
		      idb-id)))))
	#f)))

(define (fi/remove tl-widget fi-widget)
  (let ((model (tv-model fi-widget))
	(selection (tv-sel fi-widget))
	(prev-gui-cb? (gui-callback? tl-widget))
	(prev-active-filter (active-filter tl-widget)))
    (dotimes (i (gtk-tree-model-iter-n-children model #f))
      (when (iter-is-selected selection (get-iter model i))
	(let* ((iter (get-iter model i))
	       (idb-id (fiiter/get 'id model iter)))
	  (db-foliot/delete-imported-tuples idb-id #:delete-imported-db-tuple? #t))))
    (set! (gui-callback? tl-widget) #f)
    (set! (active-filter tl-widget) #t)
    (ftlw/filter-clear tl-widget 'fillcombos)
    (set! (gui-callback? tl-widget) prev-gui-cb?)
    (set! (active-filter tl-widget) prev-active-filter)
    (set! (gui-callback? fi-widget) #f)
    (fi/fill-treeview fi-widget (db-idb/select-all #t))
    (set! (gui-callback? fi-widget) #t)
    (unselect-all (tv-sel fi-widget))))

(define (fi/reimport tl-widget fi-widget)
  ;; (dimfi "ftlw/import callback called")
  (let ((model (tv-model fi-widget))
	(selection (tv-sel fi-widget))
	(prev-gui-cb? (gui-callback? tl-widget))
	(prev-active-filter (active-filter tl-widget)))
    (dotimes (i (gtk-tree-model-iter-n-children model #f))
      (when (iter-is-selected selection (get-iter model i))
	(let* ((tuples (db-idb/select-all))
	       (iter (get-iter model i))
	       (id (string->number (fiiter/get 'id model iter)))
	       (filename (fiiter/get 'filename model iter))
	       (tuple (db-idb/get-tuple tuples (db-idb/find-pos tuples 'id id =)))
	       (colour-set-id (db-idb/get tuple 'colour_set)))
	  (db-foliot/import filename colour-set-id)
	  (fiiter/set 'date model iter (date/system-date)))))
    ;; (set! (gui-callback? tl-widget) #f)
    (if prev-active-filter
	(ftlw/filter-apply tl-widget 'force)
	(begin
	  (set! (active-filter tl-widget) #t)
	  (ftlw/filter-clear tl-widget 'fillcombos)))
    ;; (set! (gui-callback? tl-widget) prev-gui-cb?)
    (set! (active-filter tl-widget) prev-active-filter)))


;;;
;;; API
;;;

(define (fi/select-gui tl-widget)
  (let* ((parent (dialog tl-widget))
	 (g-file (glade-file tl-widget))
	 (fi-widget (fi/make-dialog parent g-file))
	 (widget (dialog fi-widget)))
    (connect (add-bt fi-widget)
	     'clicked
	     (lambda (button)
	       (fi/add tl-widget fi-widget)))
    (connect (remove-bt fi-widget)
		 'clicked
		 (lambda (button)
		   (fi/remove tl-widget fi-widget)))
    (connect (reimport-bt fi-widget)
	     'clicked
	     (lambda (button)
	       (fi/reimport tl-widget fi-widget)))
    (show widget)
    (catch 'exit
	   (lambda ()
	     (let ((response (genum->symbol (make <gtk-response-type> #:value (run widget)))))
	       ;; (dimfi "fi/select-gui" response)
	       (hide widget)
	       (case response
		 ((close) ;; -7 in glade files
		  (throw 'exit 'close))
		 ((delete cancel)
		  (throw 'exit 'delete)))))
	   (lambda (key value)
	     value))))

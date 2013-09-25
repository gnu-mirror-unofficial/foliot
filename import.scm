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

  :use-module (kise config)
  :use-module (kise tl-widget)
  :use-module (kise i-dialog)
  :use-module (kise db)

  :export (ki/select-gui))


(eval-when (compile load eval)
  (re-export-public-interface (oop goops)
			      (kise i-dialog)))


(define *no-available-colour-set-msg*
  (_ "All available colour-set are already in use. For this import, we will reuse the '~A' [id '~A'] colour-set."))

(define (ki/add tl-widget ki-widget)
  (let ((model (tv-model ki-widget))
	(selection (tv-sel ki-widget))
	(prev-gui-cb? (gui-callback? tl-widget))
	(prev-active-filter (active-filter tl-widget))
	(import-filename (prompt-for-filename (dialog ki-widget)
					      (_ "Import database selection")
					      'open
					      (dirname (kcfg/get 'db-file))
					      #f)))
    (if import-filename
	(let* ((id (db-idb/get-next-id))
	       (idb-cs (modulo id (length palette)))
	       (idb-id (db-kise/import import-filename idb-cs)))
	  (if (>= id (length palette))
	      (md1b/select-gui (dialog ki-widget)
			       (_ "Warning!")
			       (_ "Colour set:")
			       (format #f "~?" *no-available-colour-set-msg* (list (colour-set-name idb-cs) idb-cs))
			       (lambda () 'nothing)
			       'dialog-warning))
	  (set! (gui-callback? tl-widget) #f)
	  (unless prev-active-filter (set! (active-filter tl-widget) #t))
	  (ktlw/filter-clear tl-widget 'fillcombos)
	  (set! (gui-callback? tl-widget) prev-gui-cb?)
	  (set! (active-filter tl-widget) prev-active-filter)
	  (ki/fill-treeview ki-widget (db-idb/select-all 'display))
	  idb-id)
	#f)))

(define (ki/remove tl-widget ki-widget)
  (let ((model (tv-model ki-widget))
	(selection (tv-sel ki-widget))
	(prev-gui-cb? (gui-callback? tl-widget))
	(prev-active-filter (active-filter tl-widget)))
    (dotimes (i (gtk-tree-model-iter-n-children model #f))
      (when (iter-is-selected selection (get-iter model i))
	(let* ((iter (get-iter model i))
	       (idb-id (kiiter/get 'id model iter)))
	  (db-kise/delete-imported-tuples idb-id #:delete-imported-db-tuple? #t))))
    (set! (gui-callback? tl-widget) #f)
    (set! (active-filter tl-widget) #t)
    (ktlw/filter-clear tl-widget 'fillcombos)
    (set! (gui-callback? tl-widget) prev-gui-cb?)
    (set! (active-filter tl-widget) prev-active-filter)
    (set! (gui-callback? ki-widget) #f)
    (ki/fill-treeview ki-widget (db-idb/select-all #t))
    (set! (gui-callback? ki-widget) #t)
    (unselect-all (tv-sel ki-widget))))

(define (ki/reimport tl-widget ki-widget)
  ;; (dimfi "ktlw/import callback called")
  (let ((model (tv-model ki-widget))
	(selection (tv-sel ki-widget))
	(prev-gui-cb? (gui-callback? tl-widget))
	(prev-active-filter (active-filter tl-widget)))
    (dotimes (i (gtk-tree-model-iter-n-children model #f))
      (when (iter-is-selected selection (get-iter model i))
	(let* ((tuples (db-idb/select-all))
	       (iter (get-iter model i))
	       (id (string->number (kiiter/get 'id model iter)))
	       (filename (kiiter/get 'filename model iter))
	       (tuple (db-idb/get-tuple tuples (db-idb/find-pos tuples 'id id =)))
	       (colour-set-id (db-idb/get tuple 'colour_set)))
	  (db-kise/import filename colour-set-id)
	  (kiiter/set 'date model iter (date/system-date)))))
    ;; (set! (gui-callback? tl-widget) #f)
    (if prev-active-filter
	(ktlw/filter-apply tl-widget 'force)
	(begin
	  (set! (active-filter tl-widget) #t)
	  (ktlw/filter-clear tl-widget 'fillcombos)))
    ;; (set! (gui-callback? tl-widget) prev-gui-cb?)
    (set! (active-filter tl-widget) prev-active-filter)))


;;;
;;; API
;;;

(define (ki/select-gui tl-widget)
  (let* ((parent (dialog tl-widget))
	 (g-file (glade-file tl-widget))
	 (ki-widget (ki/make-dialog parent g-file))
	 (widget (dialog ki-widget)))
    (connect (add-bt ki-widget)
	     'clicked
	     (lambda (button)
	       (ki/add tl-widget ki-widget)))
    (connect (remove-bt ki-widget)
		 'clicked
		 (lambda (button)
		   (ki/remove tl-widget ki-widget)))
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

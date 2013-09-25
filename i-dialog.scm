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

(define-module (kise i-dialog)
  ;; guile/guile-gnome
  :use-module (ice-9 format)
  :use-module (ice-9 receive)
  :use-module (oop goops)
  :use-module (gnome gobject)
  :use-module (gnome glade)
  :use-module (gnome gtk)
  :use-module (gnome gtk gdk-event)

  ;; common
  :use-module (macros do)
  :use-module (system i18n)
  :use-module (system aglobs)
  :use-module (gtk all)

  ;; kise
  :use-module (kise colours)
  :use-module (kise db)

  :export (*ki-widget*

	   <ki-widget>
	   gui-callback?
	   dialog
	   tv-model
	   tv-sel
	   add-bt
	   remove-bt
	   reimport-bt

	   kiiter/get
	   kiiter/set
	   ki/make-dialog
	   ki/fill-treeview))

(define kiiter/get-pos #f)
(define kiiter/get #f)
(define kiiter/set #f)

(eval-when (compile load eval)
  (let ((offsets '((icolour . 0)
		   (name . 1)
		   (date . 2)
		   (by . 3)
		   (id . 4)
		   (filename . 5)
		   (ibg . 6))))
    (set! kiiter/get-pos
	  (lambda (what) (cdr (assoc what offsets))))
    (set! kiiter/get
	  (lambda (what model iter)
	    (get-value model iter (kiiter/get-pos what))))
    (set! kiiter/set
	  (lambda (what model iter value)
	    ;; (format #t "offset: ~S~%" (cdr (assoc what offsets)))
	    (set-value model iter (kiiter/get-pos what) value))))
  (textdomain "i-dialog")
  (bindtextdomain "i-dialog" (aglobs/get 'pofdir)))


;;;
;;; Globals
;;;

(define *ki-widget* #f)


;;;
;;; Goops related
;;;

(define-class <ki-widget> ()
  (gui-callback? :accessor gui-callback? :init-keyword :gui-callback? :init-value #t)
  (xml-code :accessor xml-code :init-keyword :xml-code :init-value #f)
  (dialog :accessor dialog :init-keyword :dialog :init-value #f)

  (tv :accessor tv :init-keyword :tv :init-value #f)
  (tv-model :accessor tv-model :init-keyword :tv-model :init-value #f)
  (tv-sel :accessor tv-sel :init-keyword :tv-sel :init-value #f)

  (add-bt :accessor add-bt :init-keyword :add-bt :init-value #f)
  (remove-bt :accessor remove-bt :init-keyword :remove-bt :init-value #f)
  (reimport-bt :accessor reimport-bt :init-keyword :reimport-bt :init-value #f)

  (cancel-bt :accessor cancel-bt :init-keyword :cancel-bt :init-value #f)
  (close-bt :accessor close-bt :init-keyword :close-bt :init-value #f))


;;;
;;; Treeview related stuff
;;;

(define (ki/import-toggle-callback ki-widget model iter caller . value)
  ;; import ON
  ;;	-> 
  ;; import OFF
  ;;	-> 
  ;; import infos MUST be gathered BEFORE any setting on any toggle
  (let ((iiter-get (lambda (model iter) (kiiter/get 'import model iter)))
	(iiter-set (lambda (model iter value) (kiiter/set 'import model iter value))))
    (case caller
      ((fill) ;; dialog creation, value is not null
       (gtk2/fixed-toggled model iter iiter-get iiter-set (car value)))
      ((toggled)
       (let* ((old-value (iiter-get model iter))
	      (new-value (not old-value)))
	 (gtk2/fixed-toggled model iter iiter-get iiter-set))))))

(define (ki/add-model treeview)
  (let* ((column-types (list <gchararray>
			     <gchararray>
			     <gchararray>
			     <gchararray>
			     <gchararray>
			     <gchararray>
			     <gchararray>
			     #;<gboolean>))
	 (model (gtk-list-store-new column-types)))
    (set-model treeview model)
    (values model
	    (get-selection treeview))))

(define (ki/add-columns ki-widget treeview)
  (let* ((dpi-ratio (aglobs/get 'Xft.dpi.ratio))
	 (apply-ratio? (aglobs/get 'apply-dpi-ratio?))
	 (model (get-model treeview))
	 ;; IMPORTED ROW COLOUR
	 (renderer0 (make <gtk-cell-renderer-text>))
	 (column0   (make <gtk-tree-view-column>
		       :sizing      'fixed
		       :fixed-width 5
		       :clickable   #f
		       :resizable   #f
		       :reorderable #f
		       :alignment   .5))
	 ;; ID
	 (renderer1 (make <gtk-cell-renderer-text>))
	 (column1   (make <gtk-tree-view-column>
		      :visible     #f))
	 ;; DB NAME
	 (renderer2 (make <gtk-cell-renderer-text>))
	 (column2   (make <gtk-tree-view-column>
		      :title       (_ "Name")
		      :sizing      'autosize
		      :expand      #t
					;:sizing      'grow-only
					;:min-width   134
					;:clickable   #f
					;:resizable   #f
					;:reorderable #f
		      :alignment   .5))
	 ;; DATE
	 (renderer3 (make <gtk-cell-renderer-text>))
	 (column3   (make <gtk-tree-view-column>
		      :title       (_ "Date")
		      :sizing      'fixed
		      :fixed-width (if apply-ratio? (inexact->exact (round (* dpi-ratio 90))) 90)
		      :clickable   #f
		      :resizable   #f
		      :reorderable #f
		      :alignment   .5))
	 ;; BY
	 (renderer4 (make <gtk-cell-renderer-text>
		      #;:foreground    #;"Blue"))
	 (column4   (make <gtk-tree-view-column>
		      :title       (_ "By")
		      :sizing      'fixed
		      :fixed-width (if apply-ratio? (inexact->exact (round (* dpi-ratio 65))) 65)
		      :clickable   #f
		      :resizable   #f
		      :reorderable #f
		      :alignment   .5))
	 ;; FILENAME
	 (renderer5 (make <gtk-cell-renderer-text>))
	 (column5   (make <gtk-tree-view-column>
		      :visible     #f))
	 ;; IMPORT
	 #;(renderer9 (make <gtk-cell-renderer-toggle>))
	 #;(column9   (make <gtk-tree-view-column>
		      :title       (_ "Imp.")
		      :sizing      'fixed
		      :fixed-width 40
		      :expand      #f
		      :alignment   .5))
	 ;; ICOLOUR BACKGROUND
	 (renderer6 (make <gtk-cell-renderer-text>))
	 (column6   (make <gtk-tree-view-column>
		      :visible     #f))

	 (to-pack   `((icolour ,column0 ,renderer0 "text")
		      (name ,column2 ,renderer2 "text")
		      (date ,column3 ,renderer3 "text")
		      (by ,column4 ,renderer4 "text")
		      (id ,column1 ,renderer1 "text")
		      (fn ,column5 ,renderer5 "text")
		      (ibg ,column6 ,renderer6 "text")
		      #;(import ,column9 ,renderer9 "active"))))
    (gtk2/pack-tv-cols treeview to-pack)
    (add-attribute column0 renderer0 "cell-background" 6)
    #;(connect renderer9 ;; import
	     'toggled
	     (lambda (widget path)
	       (let ((iter (get-iter model path)))
		 (ki/import-toggle-callback ki-widget model iter 'toggled))))))

(define (ki/setup-treeview ki-widget)
  (let ((treeview (tv ki-widget)))
    (receive (model selection)
	(ki/add-model treeview)
      (set-mode selection 'multiple)
      (set! (tv-model ki-widget) model)
      (set! (tv-sel ki-widget) selection))
    (ki/add-columns ki-widget treeview)
    ki-widget))

(define (ki/fill-treeview ki-widget idb-tuples)
  (let ((model (tv-model ki-widget))
	(rem-bt (remove-bt ki-widget))
	(reimp-bt (reimport-bt ki-widget)))
    (gtk-list-store-clear model)
    (if (null? idb-tuples)
	(gtk2/set-sensitive (list rem-bt reimp-bt) #f)
	(begin
	  (gtk2/set-sensitive (list rem-bt reimp-bt) #t)
	  (for-each (lambda (idb-tuple)
		      ;; (dimfi idb-tuple)
		      (let ((iter (gtk-list-store-append model))
			    (filename (db-idb/get idb-tuple 'filename)))
					; (ki/import-toggle-callback ki-widget model iter 'fill print?)
			(kiiter/set 'name model iter (basename filename))
			(kiiter/set 'date model iter (db-idb/get idb-tuple 'imported_the))
			(kiiter/set 'by model iter (db-idb/get idb-tuple 'imported_by))
			(kiiter/set 'id model iter (number->string (db-idb/get idb-tuple 'id)))
			(kiiter/set 'filename model iter filename)
			(kiiter/set 'ibg model iter (colour-set-bg (db-idb/get idb-tuple 'colour_set)))))
	      idb-tuples)))))


;;;
;;; Making the dialog
;;;

(define (ki/make-dialog parent glade-f)
  (if *ki-widget*
      *ki-widget*
      (let* ((xmlc (glade-xml-new glade-f #f "ki/dialog"))
	     (ki-widget (make <ki-widget>
			  :xml-code xmlc
			  :dialog (get-widget xmlc "ki/dialog")
			  :tv (get-widget xmlc "ki/import_tv")
			  :add-bt (get-widget xmlc "ki/add_bt")
			  :remove-bt (get-widget xmlc "ki/remove_bt")
			  :reimport-bt (get-widget xmlc "ki/reimport_bt")
			  :cancel-bt (get-widget xmlc "ki/cancel_bt")
			  :close-bt (get-widget xmlc "ki/close_bt"))))
	(modify-bg (get-widget xmlc "ki/eventbox") 'normal *dialog-title-eb-bg*)
	(when parent (set-transient-for (dialog ki-widget) parent))
	(ki/translate ki-widget)
	(ki/setup-treeview ki-widget)

	(connect (dialog ki-widget)
		 'destroy-event
		 (lambda (widget event)
		   ;; don't do this if the dialog is launched using
		   ;; (run widget) modal 'style', it crashes
		   ;; (destroy widget)
		   (set! *ki-widget* #f)
		   #f))
	(connect (dialog ki-widget)
		 'delete-event
		 (lambda (widget event)
		   ;; same as destroy-event
		   (set! *ki-widget* #f)
		   #f))
	(connect (close-bt ki-widget)
		 'clicked
		 (lambda (button)
		   ;; same as destroy-event
		   (set! *ki-widget* #f)))

	(set! (gui-callback? ki-widget) #f)
	(gtk2/hide `(,(cancel-bt ki-widget)
		     ,(get-widget xmlc "ki/execute_tb")))
	(ki/fill-treeview ki-widget (db-idb/select-all #t))
	(set! (gui-callback? ki-widget) #t)
	(unselect-all (tv-sel ki-widget))
	(set! *ki-widget* ki-widget)
	ki-widget)))


;;;
;;; i18n - localisation
;;;

(define (ki/translate widget)
  (let ((xmlc (xml-code widget)))
    (set-title (dialog widget) (_ "Import"))
    (set-markup (get-widget xmlc "ki/title_lb")
		(format #f "<span foreground=\"darkblue\" size=\"x-large\"><b>~A</b></span>~%<b>~A</b>"
			(_ "Kisê import dialog")
			(_ "Add, remove and re-import other user's kisê database.")))))


#!

(use-modules (kise i-dialog))
(reload-module (resolve-module '(kise i-dialog)))
,m (kise i-dialog)



!#

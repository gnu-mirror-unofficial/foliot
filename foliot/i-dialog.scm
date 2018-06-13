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


(define-module (foliot i-dialog)
  #:use-module (ice-9 format)
  #:use-module (ice-9 receive)
  #:use-module (oop goops)
  #:use-module (gnome gobject)
  #:use-module (gnome glade)
  #:use-module (gnome gtk)
  #:use-module (gnome gtk gdk-event)
  #:use-module (grip module)
  #:use-module (grip iter)
  #:use-module (grip i18n)
  #:use-module (grip utils)
  #:use-module (grip gnome)
  #:use-module (foliot globals)
  #:use-module (foliot colours)
  #:use-module (foliot db)

  #:duplicates (merge-generics
		replace
		warn-override-core
		warn
		last)

  #:export (*fi-widget*

	    <fi-widget>

	    fiiter/get
	    fiiter/set
	    fi/make-dialog
	    fi/fill-treeview))


(g-export gui-callback?
          dialog
          tv-model
          tv-sel
          add-bt
          remove-bt
          reimport-bt
          cancel-bt
          close-bt)


(eval-when (expand load eval)
  (textdomain "i-dialog")
  (bindtextdomain "i-dialog" (ref %foliot-store 'pofdir)))


(define *foliot-i-dialog-offset*
  '((icolour . 0)
    (name . 1)
    (date . 2)
    (by . 3)
    (id . 4)
    (filename . 5)
    (ibg . 6)))

(define (fiiter/get-pos what)
  (assoc-ref *foliot-i-dialog-offset* what))

(define (fiiter/get what model iter)
  (get-value model iter (fiiter/get-pos what)))

(define (fiiter/set what model iter value)
  ;; (format #t "offset: ~S~%" (cdr (assoc what offsets)))
  (set-value model iter (fiiter/get-pos what) value))


;;;
;;; Globals
;;;

(define *fi-widget* #f)


;;;
;;; Goops related
;;;

(define-class <fi-widget> ()
  (gui-callback? #:accessor gui-callback? #:init-keyword #:gui-callback? #:init-value #t)
  (xml-code #:accessor xml-code #:init-keyword #:xml-code #:init-value #f)
  (dialog #:accessor dialog #:init-keyword #:dialog #:init-value #f)

  (tv #:accessor tv #:init-keyword #:tv #:init-value #f)
  (tv-model #:accessor tv-model #:init-keyword #:tv-model #:init-value #f)
  (tv-sel #:accessor tv-sel #:init-keyword #:tv-sel #:init-value #f)

  (add-bt #:accessor add-bt #:init-keyword #:add-bt #:init-value #f)
  (remove-bt #:accessor remove-bt #:init-keyword #:remove-bt #:init-value #f)
  (reimport-bt #:accessor reimport-bt #:init-keyword #:reimport-bt #:init-value #f)

  (cancel-bt #:accessor cancel-bt #:init-keyword #:cancel-bt #:init-value #f)
  (close-bt #:accessor close-bt #:init-keyword #:close-bt #:init-value #f))


;;;
;;; Treeview related stuff
;;;

(define (fi/import-toggle-callback fi-widget model iter caller . value)
  ;; import ON
  ;;	-> 
  ;; import OFF
  ;;	-> 
  ;; import infos MUST be gathered BEFORE any setting on any toggle
  (let ((iiter-get (lambda (model iter) (fiiter/get 'import model iter)))
	(iiter-set (lambda (model iter value) (fiiter/set 'import model iter value))))
    (case caller
      ((fill) ;; dialog creation, value is not null
       (gtk2/fixed-toggled model iter iiter-get iiter-set (car value)))
      ((toggled)
       (let* ((old-value (iiter-get model iter))
	      (new-value (not old-value)))
	 (gtk2/fixed-toggled model iter iiter-get iiter-set))))))

(define (fi/add-model treeview)
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

(define (fi/add-columns fi-widget treeview)
  (let* ((dpi-ratio (ref %xft-store 'scale-factor))
         (apply-ratio? (ref %xft-store 'apply-scale-factor?))
	 (model (get-model treeview))
	 ;; IMPORTED ROW COLOUR
	 (renderer0 (make <gtk-cell-renderer-text>))
	 (column0   (make <gtk-tree-view-column>
		       #:sizing      'fixed
		       #:fixed-width 5
		       #:clickable   #f
		       #:resizable   #f
		       #:reorderable #f
		       #:alignment   .5))
	 ;; ID
	 (renderer1 (make <gtk-cell-renderer-text>))
	 (column1   (make <gtk-tree-view-column>
		      #:visible     #f))
	 ;; DB NAME
	 (renderer2 (make <gtk-cell-renderer-text>))
	 (column2   (make <gtk-tree-view-column>
		      #:title       (_ "Name")
		      #:sizing      'autosize
		      #:expand      #t
					;#:sizing      'grow-only
					;#:min-width   134
					;#:clickable   #f
					;#:resizable   #f
					;#:reorderable #f
		      #:alignment   .5))
	 ;; DATE
	 (renderer3 (make <gtk-cell-renderer-text>))
	 (column3   (make <gtk-tree-view-column>
		      #:title       (_ "Date")
		      #:sizing      'fixed
		      #:fixed-width (if apply-ratio?
					(inexact->exact (round (* dpi-ratio 90)))
					90)
		      #:clickable   #f
		      #:resizable   #f
		      #:reorderable #f
		      #:alignment   .5))
	 ;; BY
	 (renderer4 (make <gtk-cell-renderer-text>
		      #;#:foreground    #;"Blue"))
	 (column4   (make <gtk-tree-view-column>
		      #:title       (_ "By")
		      #:sizing      'fixed
		      #:fixed-width (if apply-ratio?
					(inexact->exact (round (* dpi-ratio 65)))
					65)
		      #:clickable   #f
		      #:resizable   #f
		      #:reorderable #f
		      #:alignment   .5))
	 ;; FILENAME
	 (renderer5 (make <gtk-cell-renderer-text>))
	 (column5   (make <gtk-tree-view-column>
		      #:visible     #f))
	 ;; IMPORT
	 #;(renderer9 (make <gtk-cell-renderer-toggle>))
	 #;(column9   (make <gtk-tree-view-column>
		      #:title       (_ "Imp.")
		      #:sizing      'fixed
		      #:fixed-width 40
		      #:expand      #f
		      #:alignment   .5))
	 ;; ICOLOUR BACKGROUND
	 (renderer6 (make <gtk-cell-renderer-text>))
	 (column6   (make <gtk-tree-view-column>
		      #:visible     #f))

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
		 (fi/import-toggle-callback fi-widget model iter 'toggled))))))

(define (fi/setup-treeview fi-widget)
  (let ((treeview (tv fi-widget)))
    (receive (model selection)
	(fi/add-model treeview)
      (set-mode selection 'multiple)
      (set! (tv-model fi-widget) model)
      (set! (tv-sel fi-widget) selection))
    (fi/add-columns fi-widget treeview)
    fi-widget))

(define (fi/fill-treeview fi-widget idb-tuples)
  (let ((model (tv-model fi-widget))
	(rem-bt (remove-bt fi-widget))
	(reimp-bt (reimport-bt fi-widget)))
    (gtk-list-store-clear model)
    (if (null? idb-tuples)
	(gtk2/set-sensitive (list rem-bt reimp-bt) #f)
	(begin
	  (gtk2/set-sensitive (list rem-bt reimp-bt) #t)
	  (for-each (lambda (idb-tuple)
		      ;; (dimfi idb-tuple)
		      (let ((iter (gtk-list-store-append model))
			    (filename (db-idb/get idb-tuple 'filename)))
					; (fi/import-toggle-callback fi-widget model iter 'fill print?)
			(fiiter/set 'name model iter (basename filename))
			(fiiter/set 'date model iter (db-idb/get idb-tuple 'imported_the))
			(fiiter/set 'by model iter (db-idb/get idb-tuple 'imported_by))
			(fiiter/set 'id model iter (number->string (db-idb/get idb-tuple 'id)))
			(fiiter/set 'filename model iter filename)
			(fiiter/set 'ibg model iter (color-set-bg (db-idb/get idb-tuple 'colour_set)))))
	      idb-tuples)))))


;;;
;;; Making the dialog
;;;

(define (fi/make-dialog parent glade-f)
  (if *fi-widget*
      *fi-widget*
      (let* ((xmlc (glade-xml-new glade-f #f "fi/dialog"))
	     (fi-widget (make <fi-widget>
			  #:xml-code xmlc
			  #:dialog (get-widget xmlc "fi/dialog")
			  #:tv (get-widget xmlc "fi/import_tv")
			  #:add-bt (get-widget xmlc "fi/add_bt")
			  #:remove-bt (get-widget xmlc "fi/remove_bt")
			  #:reimport-bt (get-widget xmlc "fi/reimport_bt")
			  #:cancel-bt (get-widget xmlc "fi/cancel_bt")
			  #:close-bt (get-widget xmlc "fi/close_bt"))))
	(modify-bg (get-widget xmlc "fi/eventbox") 'normal *dialog-title-eb-bg*)
	(when parent (set-transient-for (dialog fi-widget) parent))
	(fi/translate fi-widget)
	(fi/setup-treeview fi-widget)

	(connect (dialog fi-widget)
		 'destroy-event
		 (lambda (widget event)
		   ;; don't do this if the dialog is launched using
		   ;; (run widget) modal 'style', it crashes
		   ;; (destroy widget)
		   (set! *fi-widget* #f)
		   #f))
	(connect (dialog fi-widget)
		 'delete-event
		 (lambda (widget event)
		   ;; same as destroy-event
		   (set! *fi-widget* #f)
		   #f))
	(connect (close-bt fi-widget)
		 'clicked
		 (lambda (button)
		   ;; same as destroy-event
		   (set! *fi-widget* #f)))

	(set! (gui-callback? fi-widget) #f)
	(gtk2/hide `(,(cancel-bt fi-widget)
		     ,(get-widget xmlc "fi/execute_tb")))
	(fi/fill-treeview fi-widget (db-idb/select-all #t))
	(set! (gui-callback? fi-widget) #t)
	(unselect-all (tv-sel fi-widget))
	(set! *fi-widget* fi-widget)
	fi-widget)))


;;;
;;; i18n - localisation
;;;

(define (fi/translate widget)
  (let ((xmlc (xml-code widget)))
    (set-title (dialog widget) (_ "Import"))
    (set-markup (get-widget xmlc "fi/title_lb")
		(format #f "<span foreground=\"darkblue\" size=\"x-large\"><b>~A</b></span>~%<b>~A</b>"
			(_ "GNU Foliot import dialog")
			(_ "Add, remove and re-import other GNU Foliot user's database(s).")))))

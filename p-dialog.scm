;; -*- mode: scheme; coding: utf-8 -*-

;;;; Copyright (C) 2011, 2012
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

(define-module (kise p-dialog)
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
  :use-module (macros when)
  :use-module (system i18n)
  :use-module (system aglobs)
  :use-module (gtk all)

  ;; kise
  :use-module (kise colours)
  :use-module (kise db-kise)
  :use-module (kise db-printing-templates)

  :export (*kp-widget*
	   kp/make-dialog
	   <kp-widget>
	   tpl-tuples
	   dialog
	   printer-combo
	   pdf
	   tex
	   items
	   mode
	   template-combo
	   kp/get-core-ltx-field-specs))


(define kpiter/get-pos #f)
(define kpiter/get #f)
(define kpiter/set #f)

(eval-when (compile load eval)
  (let ((offsets '((print . 0)
		   (group . 1)
		   (name . 2)
		   (asc . 3)
		   (desc . 4)
		   (none . 5))))
    (set! kpiter/get-pos
	  (lambda (what) (cdr (assoc what offsets))))
    (set! kpiter/get
	  (lambda (what model iter)
	    (get-value model iter (kpiter/get-pos what))))
    (set! kpiter/set
	  (lambda (what model iter value)
	    ;; (format #t "offset: ~S~%" (cdr (assoc what offsets)))
	    (set-value model iter (kpiter/get-pos what) value)))
  (textdomain "p-dialog")
  (bindtextdomain "p-dialog" (aglobs/get 'pofdir))))


;;;
;;; Globals
;;;

(define *kp-widget* #f)

;; debug 'mode'
(define kp-widget #f)
(define model #f)
(define treeview #f)
(define selection #f)

(define (kp/set-debug-variables)
  (set! kp-widget *kp-widget*)
  (set! treeview (g-tv kp-widget))
  (set! model (g-tv-model kp-widget))
  (set! selection (g-tv-sel kp-widget)))


;;;
;;; Goops related
;;;

(define-class <kp-widget> ()
  (gui-callback? :accessor gui-callback? :init-keyword :gui-callback? :init-value #t)
  (tpl-tuples :accessor tpl-tuples :init-keyword :tpl-tuples :init-value #f)
  (xml-code :accessor xml-code :init-keyword :xml-code :init-value #f)
  (dialog :accessor dialog :init-keyword :dialog :init-value #f)
  (printer-frame-lb :accessor printer-frame-lb :init-keyword :printer-frame-lb :init-value #f)
  (printer-combo :accessor printer-combo :init-keyword :printer-combo :init-value #f)
  (pdf-cb :accessor pdf-cb :init-keyword :pdf-cb :init-value #f)
  (pdf :accessor pdf :init-keyword :pdf :init-value #f)
  (latex-keep-files-cb :accessor latex-keep-files-cb :init-keyword :latex-keep-files-cb :init-value #f)
  (tex :accessor tex :init-keyword :tex :init-value #f)
  (template-frame-lb :accessor template-frame-lb :init-keyword :template-frame-lb :init-value #f)
  (template-add-bt :accessor template-add-bt :init-keyword :template-add-bt :init-value #f)
  (template-rem-bt :accessor template-rem-bt :init-keyword :template-rem-bt :init-value #f)
  (template-name-lb :accessor template-name-lb :init-keyword :template-name-lb :init-value #f)
  (template-combo :accessor template-combo :init-keyword :template-combo :init-value #f)
  (template-entry :accessor template-entry :init-keyword :template-entry :init-value #f)
  (template-entry-focus-in-value :accessor template-entry-focus-in-value :init-value #f)
  (tpl-active-pos-at-entry-focus-in :accessor tpl-active-pos-at-entry-focus-in :init-value #f)
  (i-selected-rb :accessor i-selected-rb :init-keyword :i-selected-rb :init-value #f)
  (i-all-rb :accessor i-all-rb :init-keyword :i-all-rb :init-value #f)
  (items :accessor items :init-keyword :items :init-value #f)
  (m-draft-rb :accessor m-draft-rb :init-keyword :m-draft-rb :init-value #f)
  (m-com-rb :accessor m-com-rb :init-keyword :m-com-rb :init-value #f)
  (mode :accessor mode :init-keyword :mode :init-value #f)
  (g-tv :accessor g-tv :init-keyword :g-tv :init-value #f)
  (g-tv-model :accessor g-tv-model :init-keyword :g-tv-model :init-value #f)
  (g-tv-sel :accessor g-tv-sel :init-keyword :g-tv-sel :init-value #f)
  (grouping :accessor grouping :init-keyword :grouping :init-value #f)
  (g-add-bt :accessor g-add-bt :init-keyword :g-add-bt :init-value #f)
  (g-rem-bt :accessor g-rem-bt :init-keyword :g-rem-bt :init-value #f)
  (g-up-bt :accessor g-up-bt :init-keyword :g-up-bt :init-value #f)
  (g-down-bt :accessor g-down-bt :init-keyword :g-down-bt :init-value #f)
  (g-reselect-path? :accessor g-reselect-path? :init-value #f)
  (cancel-bt :accessor cancel-bt :init-keyword :cancel-bt :init-value #f)
  (print-bt :accessor print-bt :init-keyword :print-bt :init-value #f))


;;;
;;; Treeview related stuff
;;;

(define (kp/template-entry-focus-out-callback kp-widget . caller)
  ;; (format #t "focus-out-callback,~%  guicb?: ~S, caller: ~S~%" (gui-callback? kp-widget) caller)
  (when (gui-callback? kp-widget)
    (let* ((tpl-combo (template-combo kp-widget))
	   (active (get-active tpl-combo))
	   (value (get-text (template-entry kp-widget))))
      (if (= active -1)
	  (begin
	    (set! (gui-callback? kp-widget) #f)
	    (kp/update kp-widget 'name value (tpl-active-pos-at-entry-focus-in kp-widget))
	    (kp/fill-templates-combo kp-widget 'reload)
	    (let ((tpl-pos (gtk2/combo-find-row tpl-combo value)))
	      ;; (format #t "  new-value: ~a, new-pos: ~A~%" value tpl-pos)
	      (set-active tpl-combo tpl-pos)
	      (set! (tpl-active-pos-at-entry-focus-in kp-widget) tpl-pos))
	    (set! (gui-callback? kp-widget) #t))
	  ;; (format #t "  no update~%")
	  )))
  ;; gtk2 requirement ...
  #f)

(define (kp/on-g-tv-row-change kp-widget)
  ;; (format #t "kp/on-g-tv-row-change ...~%")
  (let* ((kp-dialog (dialog kp-widget))
	 (tpl-entry (template-entry kp-widget))
	 (focusw (get-focus kp-dialog)))
    (when (and focusw (eq? focusw tpl-entry))
      (kp/template-entry-focus-out-callback kp-widget))))

(define (kp/row-move kp-widget model iter toggle new-value
		     nb-rows first-grouped last-grouped last-printed)		     
  (let* ((tv-sel (g-tv-sel kp-widget))
	 (old-pos (car (get-path model iter)))
	 (new-pos (case toggle
		    ((print) (if new-value
				 (if last-printed (1+ last-printed) 0)
				 (if last-printed last-printed old-pos)))
		    ((group) (if new-value
				 (if first-grouped (1+ last-grouped) 0)
				 (if last-grouped last-grouped old-pos)))))
	 (new-path (list new-pos)))
    ;; (format #t "Toggle: ~A, Old pos: ~A, New pos: ~S~%" toggle old-pos new-pos)
    (if (= new-pos old-pos)
	(kp/check-g-up-down-sensitive-needs kp-widget model (get-iter model new-pos) new-pos toggle)
	;; (set! (gui-callback? kp-widget) #f)
	(begin
	  (unselect-all tv-sel)
	  (if (< new-pos old-pos)
	      (move-before model
			   iter
			   (get-iter model new-path))
	      (move-after model
			  iter
			  (get-iter model new-path)))
	  (set! (g-reselect-path? kp-widget) new-path))
	;; (set! (gui-callback? kp-widget) #t)
	)))

(define (kp/check-print-bt-sensitiveness kp-widget)
  (let* ((model (g-tv-model kp-widget))
	 (nb-rows (gtk-tree-model-iter-n-children model #f))
	 (nb-print 0))
    (catch 'exit
      (lambda ()
	(dotimes (i nb-rows)
	  (let ((iter (get-iter model (list i))))
	    (if (kpiter/get 'print model iter)
		(set! nb-print (1+ nb-print))
		(throw 'exit #f)))))
      (lambda (key index) index))
    (if (> nb-print 0)
	(set-sensitive (print-bt kp-widget) #t)
	(set-sensitive (print-bt kp-widget) #f))))

(define (kp/printing-toggle-callback kp-widget model iter caller . value)
  ;; print ON
  ;;	-> does not affect other options
  ;;	-> row moved after the last row with print ON
  ;; print OFF
  ;; 	-> group OFF as well
  ;;	-> row moved after the last row with print ON [too]
  ;; grouping infos MUST be gathered BEFORE any setting on any toggle
  (receive (nb-rows first-grouped last-grouped last-printed)
      (if (gui-callback? kp-widget) (kp/get-grouping-infos kp-widget) (values -1 #f #f #f))
    (let* ((piter-get (lambda (model iter) (kpiter/get 'print model iter)))
	   (piter-set (lambda (model iter value) (kpiter/set 'print model iter value)))
	   (giter-get (lambda (model iter) (kpiter/get 'group model iter)))
	   (giter-set (lambda (model iter value) (kpiter/set 'group model iter value)))
	   (new-value (if (null? value)
			  (gtk2/fixed-toggled model iter piter-get piter-set)
			  (gtk2/fixed-toggled model iter piter-get piter-set (car value)))))
      (when (gui-callback? kp-widget)
	(kp/on-g-tv-row-change kp-widget) ;; <- making sure focus-out-event tpl-entry ...
	(kp/row-move kp-widget model iter 'print new-value
		     nb-rows first-grouped last-grouped last-printed)
	(unless new-value (gtk2/fixed-toggled model iter giter-get giter-set #f))
	(kp/check-print-bt-sensitiveness kp-widget)
	(kp/update-grouping kp-widget)))))

(define (kp/grouping-toggle-callback kp-widget model iter caller . value)
  ;; group ON
  ;;	-> print ON as well
  ;;	-> row moved after the last row with group ON
  ;; group OFF
  ;;	-> does not affect other options
  ;;	-> row moved after the last row with group ON
  ;; grouping infos MUST be gathered BEFORE any setting on any toggle
  (receive (nb-rows first-grouped last-grouped last-printed)
      (if (gui-callback? kp-widget) (kp/get-grouping-infos kp-widget) (values -1 #f #f #f))
    (let* ((piter-get (lambda (model iter) (kpiter/get 'print model iter)))
	   (piter-set (lambda (model iter value) (kpiter/set 'print model iter value)))
	   (giter-get (lambda (model iter) (kpiter/get 'group model iter)))
	   (giter-set (lambda (model iter value) (kpiter/set 'group model iter value)))
	   (new-value (if (null? value)
			  (gtk2/fixed-toggled model iter giter-get giter-set)
			  (gtk2/fixed-toggled model iter giter-get giter-set (car value)))))
      (when (gui-callback? kp-widget)
	(kp/on-g-tv-row-change kp-widget) ;; <- making sure focus-out-event tpl-entry ...
	(kp/row-move kp-widget model iter 'group new-value
		     nb-rows first-grouped last-grouped last-printed)
	(when new-value 
	  (gtk2/fixed-toggled model iter piter-get piter-set #t)
	  (kp/check-print-bt-sensitiveness kp-widget))
	(kp/update-grouping kp-widget)))))

(define (kp/sorting-radios-callback kp-widget model iter mode)
  (let* (;; (row (string->number path)) 
	 ;; (current-row tl-widget)) ;; <- this is not correct the
	 ;; user may click the checkbox of another iter AND this
	 ;; callback seems to be called before the row-changed one!
	 (asc-get (lambda (model iter) (kpiter/get 'asc model iter)))
	 (asc-set (lambda (model iter value) (kpiter/set 'asc model iter value)))
	 (desc-get (lambda (model iter) (kpiter/get 'desc model iter)))
	 (desc-set (lambda (model iter value) (kpiter/set 'desc model iter value)))
	 (none-get (lambda (model iter) (kpiter/get 'none model iter)))
	 (none-set (lambda (model iter value) (kpiter/set 'none model iter value))))
    ;; (format #t "sorting signal...~%")
    (case mode
      ((asc) (unless (asc-get model iter)
	       (let ((new-value (gtk2/fixed-toggled model iter asc-get asc-set)))
		 (gtk2/fixed-toggled model iter desc-get desc-set (not new-value))
		 (gtk2/fixed-toggled model iter none-get none-set (not new-value))
		 )))
      ((desc) (unless (desc-get model iter)
		(let ((new-value (gtk2/fixed-toggled model iter desc-get desc-set)))
		  (gtk2/fixed-toggled model iter asc-get asc-set (not new-value))
		  (gtk2/fixed-toggled model iter none-get none-set (not new-value))
		  )))
      ((none) (unless (none-get model iter)
		(let ((new-value (gtk2/fixed-toggled model iter none-get none-set)))
		  (gtk2/fixed-toggled model iter asc-get asc-set (not new-value))
		  (gtk2/fixed-toggled model iter desc-get desc-set (not new-value))
		  ))))
    (when (gui-callback? kp-widget) 
      (kp/on-g-tv-row-change kp-widget)
      (kp/update-grouping kp-widget))))

(define (kp/add-g-model treeview)
  (let* ((column-types (list <gboolean>
			     <gboolean>
			     <gchararray>
			     <gboolean>
			     <gboolean>
			     <gboolean>))
	 (model (gtk-list-store-new column-types)))
    (set-model treeview model)
    (values model
	    (get-selection treeview))))

(define (kp/add-g-columns kp-widget treeview)
  (let* ((model     (get-model treeview))
	 ;; GROUPING
	 (renderer1 (make <gtk-cell-renderer-toggle>))
	 (column1   (make <gtk-tree-view-column>
		      :title       (_ "Grp.")
		      :sizing      'fixed
		      :fixed-width 40
		      :expand      #f
		      :alignment   .5))
	 ;; WHAT
	 (renderer2 (make <gtk-cell-renderer-text>))
	 (column2   (make <gtk-tree-view-column>
		      :title       (_ "What")
		      :sizing      'autosize
		      :expand      #t
		      ;:sizing      'grow-only
		      ;:min-width   134
		      ;:clickable   #f
		      ;:resizable   #f
		      ;:reorderable #f
		      :alignment   .5))
	 ;; ASCENDING
	 (renderer3 (make <gtk-cell-renderer-toggle>
		      :radio #t))
	 (column3   (make <gtk-tree-view-column>
		      :title       (_ "Asc.")
		      :sizing      'fixed
		      :fixed-width 45
		      :expand      #f
		      :alignment   .5))
	 ;; DESCENDING
	 (renderer4 (make <gtk-cell-renderer-toggle>
		      :radio #t))
	 (column4   (make <gtk-tree-view-column>
		      :title       (_ "Desc.")
		      :sizing      'fixed
		      :fixed-width 45
		      :expand      #f
		      :alignment   .5))
	 ;; NONE
	 (renderer5 (make <gtk-cell-renderer-toggle>
		      :radio #t))
	 (column5   (make <gtk-tree-view-column>
		      :title       (_ "None")
		      :sizing      'fixed
		      :fixed-width 45
		      :expand      #f
		      :alignment   .5))
	 ;; PRINTING
	 (renderer6 (make <gtk-cell-renderer-toggle>))
	 (column6   (make <gtk-tree-view-column>
		      :title       (_ "Print")
		      :sizing      'fixed
		      :fixed-width 45
		      :expand      #f
		      :alignment   .5))
	 (to-pack   `((printing ,column6 ,renderer6 "active")
		      (grouping ,column1 ,renderer1 "active")
		      (what ,column2 ,renderer2 "text")
		      (ascending ,column3 ,renderer3 "active")
		      (descending ,column4 ,renderer4 "active")
		      (none ,column5 ,renderer5 "active"))))
    (gtk2/pack-tv-cols treeview to-pack)
    (connect renderer6 ;; print
	     'toggled
	     (lambda (widget path)
	       ;; (format #t "print-toggle: ~S, path: ~S~%" widget path)
	       (let ((iter (get-iter model path)))
		 (kp/printing-toggle-callback kp-widget model iter 'toggled))))
    (connect renderer1 ;; grouping
	     'toggled
	     (lambda (widget path)
	       ;; (format #t "group-toggle: ~S, path: ~S~%" widget path)
	       (let ((iter (get-iter model path)))
		 (kp/grouping-toggle-callback kp-widget model iter 'toggled))))
    (connect renderer3 ;; sorting, ascending
	     'toggled
	     (lambda (widget path)
	       (let ((iter (get-iter model path)))
		 (kp/sorting-radios-callback kp-widget model iter 'asc))))
    (connect renderer4 ;; sorting, descending
	     'toggled
	     (lambda (widget path)
	       (let ((iter (get-iter model path)))
		 (kp/sorting-radios-callback kp-widget model iter 'desc))))
    (connect renderer5 ;; sorting, none
	     'toggled
	     (lambda (widget path)
	       (let ((iter (get-iter model path)))
		 (kp/sorting-radios-callback kp-widget model iter 'none))))))

(define (kp/setup-g-treeview kp-widget)
  (let ((treeview (g-tv kp-widget)))
    (receive (model selection)
	(kp/add-g-model treeview)
      (set-mode selection 'single)
      (set! (g-tv-model kp-widget) model)
      (set! (g-tv-sel kp-widget) selection))
    (kp/add-g-columns kp-widget treeview)
    kp-widget))

(define (kp/fill-g-treeview kp-widget items)
  ;; Note: whenever the structure of db-pt/default-fields changes,
  ;;       this needs to be edited too
  (let ((model (g-tv-model kp-widget)))
    (gtk-list-store-clear model)
    (for-each (lambda (item)
		(let* ((iter (gtk-list-store-append model))
		       (print? (db-pt/df-get 'print item))
		       (group? (db-pt/df-get 'group item))
		       (name (db-pt/df-get 'name item))
		       (sort-mode (db-pt/df-get 'sort item)))
		  (kp/printing-toggle-callback kp-widget model iter 'fill print?)
		  (kp/grouping-toggle-callback kp-widget model iter 'fill group?)
		  (kpiter/set 'name model iter name)
		  (case sort-mode
		    ((asc) (kp/sorting-radios-callback kp-widget model iter 'asc))
		    ((desc) (kp/sorting-radios-callback kp-widget model iter 'desc))
		    ((none) (kp/sorting-radios-callback kp-widget model iter 'none))
		    (else
		     (format #t "Unkown sorting mode ~S~%" sort-mode)))))
	items)))

(define (kp/check-template-toolbar-buttons-sensitivity kp-widget)
  (let ((tpl-tuple-nb (length (tpl-tuples kp-widget))))
    (case (length (tpl-tuples kp-widget))
      ((0 1) (set-sensitive (template-rem-bt kp-widget) #f))
      (else
       (set-sensitive (template-rem-bt kp-widget) #t)))))

(define (kp/set-gtk-entries kp-widget)
  (let* ((tpl-pos (get-active (template-combo kp-widget)))
	 (tpl-tuple (db-pt/get-tuple (tpl-tuples kp-widget) tpl-pos))
	 (tpl-items (db-pt/get tpl-tuple 'items))
	 (tpl-mode (db-pt/get tpl-tuple 'mode))
	 (tpl-grouping-str (db-pt/get tpl-tuple 'group_and_sort))
	 (tpl-grouping (with-input-from-string tpl-grouping-str read))
	 (prev-gui-cb? (gui-callback? kp-widget)))
    ;; (format #t "kp/set-gtk-entries, tpl-pos ~S~%tuple: ~S~%" tpl-pos tpl-tuple)
    (set! (gui-callback? kp-widget) #f)
    (kp/check-template-toolbar-buttons-sensitivity kp-widget)
    (case (string->symbol tpl-items)
      ((selected)
       (set-active (i-selected-rb kp-widget) #t)
       (emit (i-selected-rb kp-widget) 'toggled))
      ((all) 
       (set-active (i-all-rb kp-widget) #t)
       (emit (i-all-rb kp-widget) 'toggled)))
    (case (string->symbol tpl-mode)
      ((draft)
       (set-active (m-draft-rb kp-widget) #t)
       (emit (m-draft-rb kp-widget) 'toggled))
      ((commercial)
       (set-active (m-com-rb kp-widget) #t)
       (emit (m-com-rb kp-widget) 'toggled)))
    (kp/fill-g-treeview kp-widget tpl-grouping)
    (set! (tpl-active-pos-at-entry-focus-in kp-widget) tpl-pos)
    (set! (gui-callback? kp-widget) prev-gui-cb?)))


;;;
;;; Printers
;;;

(define (kp/fill-printers-combo kp-widget)
  #t)


;;;
;;; Templates
;;;

(define (kp/get-templates)
  (let ((tuples (db-pt/select-all)))
    (if (null? tuples)
	(begin
	  (db-pt/add-default)
	  (db-pt/select-all))
	tuples)))

(define (kp/get-template-names kp-widget)
  (let ((values (list)))
    (for-each (lambda (tpl-tuple)
		(set! values (cons (db-pt/get tpl-tuple 'name) values)))
	(tpl-tuples kp-widget))
    (reverse! values)))

(define (kp/fill-templates-combo kp-widget . reload?)
  (let ((prev-gui-cb? (gui-callback? kp-widget)))
    (when (not (null? reload?))
      (set! (tpl-tuples kp-widget) (kp/get-templates)))
    (set! (gui-callback? kp-widget) #f)
    (gtk2/fill-combo (template-combo kp-widget) (kp/get-template-names kp-widget))
    (set! (gui-callback? kp-widget) prev-gui-cb?)))

(define (kp/update kp-widget what value . row)
  (let* ((tpl-pos (if (null? row) (get-active (template-combo kp-widget)) (car row)))
	 (tpl-tuple (db-pt/get-tuple (tpl-tuples kp-widget) tpl-pos)))
    ;; (format #t "kp/update: tpl-pos ~A~%  before: : ~S~%" tpl-pos tpl-tuple)
    (db-pt/update tpl-tuple what value)
    ;; (format #t "  after: ~S~%" tpl-tuple)
    ))

(define (kp/build-grouping-value kp-widget)
  (let* ((model (g-tv-model kp-widget))
	 (nb-rows (gtk-tree-model-iter-n-children model #f))
	 (value (list)))
    (dotimes (i nb-rows)
      (let* ((iter (get-iter model (list i)))
	     (print? (kpiter/get 'print model iter))
	     (group? (kpiter/get 'group model iter))
	     (name (kpiter/get 'name model iter))
	     (asc? (kpiter/get 'asc model iter))
	     (desc? (kpiter/get 'desc model iter))
	     (none? (kpiter/get 'none model iter)))
	;; (format #t "Row: ~A, ter: ~S~%" i iter)
	(set! value
	      (cons (list print? group? name
			  (cond (asc? 'asc) (desc? 'desc) (none? 'none)))
		    value))))
    (reverse! value)))

(define (kp/update-grouping kp-widget)
  (let ((value (kp/build-grouping-value kp-widget)))
    ;; (format #t "Grouping: ~S~%" value)
    (kp/update kp-widget 'group_and_sort (format #f "~S" value))))


;;;
;;; Grouping and sorting treeview up and down callbacks
;;;

(define (kp/grouping-up kp-widget)
  ;; (format #t "Grouping up has been clicked~%")
  (let ((tv-sel (g-tv-sel kp-widget)))
    (receive (model iter)
	(get-selected tv-sel)
      (let* ((old-pos (car (get-path model iter)))
	     (new-pos (1- old-pos))
	     (new-path (list new-pos)))
	(move-before model
		     iter
		     (get-iter model new-path))
	(set! (gui-callback? kp-widget) #f)
	(unselect-all tv-sel)
	(select-path tv-sel new-path)
	(kp/update-grouping kp-widget)
	(kp/check-g-up-down-sensitive-needs kp-widget model (get-iter model new-pos) new-pos)
	(set! (gui-callback? kp-widget) #t)))))

(define (kp/grouping-down kp-widget)
  ;; (format #t "Grouping up has been clicked~%")
  (let ((tv-sel (g-tv-sel kp-widget)))
    (receive (model iter)
	(get-selected tv-sel)
      (let* ((old-pos (car (get-path model iter)))
	     (new-pos (1+ old-pos))
	     (new-path (list new-pos)))
	(move-after model
		    iter
		    (get-iter model new-path))
	(set! (gui-callback? kp-widget) #f)
	(unselect-all tv-sel)
	(select-path tv-sel new-path)
	(kp/update-grouping kp-widget)
	(kp/check-g-up-down-sensitive-needs kp-widget model (get-iter model new-pos) new-pos)
	(set! (gui-callback? kp-widget) #t)))))

(define (kp/get-grouping-infos kp-widget)
  ;; if grouped items, per definition the first is at pos 0, and only
  ;; in this case do we search the pos of the last, starting from the
  ;; bottom.
  (let* ((model (g-tv-model kp-widget))
	 (nb-rows (gtk-tree-model-iter-n-children model #f))
	 (first-grouped (if (kpiter/get 'group model (get-iter model 0)) 0 #f))
	 (last-grouped #f)
	 (last-printed #f))
    ;; the last grouped, if any, is always above [or is] the last
    ;; printed
    (set! last-grouped
	  (catch 'exit
	    (lambda ()
	      (do ((i (1- nb-rows) (1- i)))
		  ((< i 0) (throw 'exit #f))
		(let ((iter (get-iter model i)))
		  ;; (format #t "g-infos, row: ~A, print?: ~S, group?: ~S~%"
		  ;; 	  i (kpiter/get 'print model iter) (kpiter/get 'group model iter))
		  (unless last-printed
		    (if (kpiter/get 'print model iter) (set! last-printed i)))
		  (if (kpiter/get 'group model iter)
		      (throw 'exit i)))))
	    (lambda (key index) index)))
    (values nb-rows first-grouped last-grouped last-printed)))

(define (kp/display-get-grouping-debug-infos nb-rows first-grouped last-grouped last-printed caller)
  (format #t "Nb-Rows: ~A, First grouped: ~S, Last grouped: ~S, Last printed: ~S, Caller: ~S~%"
	  nb-rows first-grouped last-grouped last-printed caller))

(define (kp/check-g-up-down-sensitive-needs kp-widget model iter row . caller)
  (let ((up-bt (g-up-bt kp-widget))
	(down-bt (g-down-bt kp-widget)))
    (receive (nb-rows first-grouped last-grouped last-printed)
	(kp/get-grouping-infos kp-widget)
      ;; (kp/display-get-grouping-debug-infos nb-rows first-grouped last-grouped last-printed caller)
      (set-sensitive up-bt
		     (if (or (= row 0)
			     (and last-grouped
				  (= row (1+ last-grouped)))
			     (and last-printed
				  (= row (1+ last-printed))))
			 #f #t))
      (set-sensitive down-bt
		     (if (or (and last-grouped
				  (= row last-grouped))
			     (and last-printed
				  (= row last-printed))
			     (= row (1- nb-rows)))
			 #f #t)))))


;;;
;;; Delete
;;;

(define (kp/delete-msg-str)
  ;; with "~10,,,' @A": see comment in tl-widget.scm
  (string-append (_ "Are you sure you want to delete this template ?")
		 "~%~%	~10,,,' A: ~A"))

(define (kp/delete kp-widget)
  (let* ((tpl-pos (get-active (template-combo kp-widget)))
	 (tpl-tuple (db-pt/get-tuple (tpl-tuples kp-widget) tpl-pos))
	 (tpl-id (db-pt/get tpl-tuple 'id))
	 (tpl-name (db-pt/get tpl-tuple 'name)))
    (md2b/select-gui (dialog kp-widget)
		     (_ "Confirm dialog")
		     (_ "Deletion")
		     (format #f "~?" (kp/delete-msg-str)
			     (list (_ "Name") tpl-name))
		     (lambda ()
		       (db-pt/delete tpl-id)
		       (let* ((dummy (kp/fill-templates-combo kp-widget 'reload))
			      (tpl-nb (length (tpl-tuples kp-widget))))
			 (if (>= tpl-pos (1- tpl-nb))
			     (set-active (template-combo kp-widget) (1- tpl-nb))
			     (set-active (template-combo kp-widget) tpl-pos))))
		     (lambda () 'nothing))))


;;;
;;; Making the dialog
;;;

(define (kp/make-dialog parent glade-f)
  (if *kp-widget*
      *kp-widget*
      (let* ((xmlc (glade-xml-new glade-f #f "kp/dialog"))
	     (kp-widget (make <kp-widget>
			  :tpl-tuples (kp/get-templates)
			  :xml-code xmlc
			  :dialog (get-widget xmlc "kp/dialog")
			  :printer-frame-lb (get-widget xmlc "kp/printer_frame_lb")
			  :printer-combo (get-widget xmlc "kp/printer_combo")
			  :pdf-cb (get-widget xmlc "kp/pdf_cb")
			  :latex-keep-files-cb (get-widget xmlc "kp/latex_keep_files_cb")
			  :template-add-bt (get-widget xmlc "kp/template_add_bt")
			  :template-rem-bt (get-widget xmlc "kp/template_rem_bt")
			  :template-combo (get-widget xmlc "kp/template_combo")
			  :template-entry (gtk-bin-get-child (get-widget xmlc "kp/template_combo"))
			  :i-selected-rb (get-widget xmlc "kp/items_entry_rb")
			  :i-all-rb (get-widget xmlc "kp/items_all_rb")
			  :m-draft-rb (get-widget xmlc "kp/mode_draft_rb")
			  :m-com-rb (get-widget xmlc "kp/mode_commercial_rb")
			  :g-tv (get-widget xmlc "kp/grouping_tv")
			  :g-add-bt (get-widget xmlc "kp/grouping_add_bt")
			  :g-rem-bt (get-widget xmlc "kp/grouping_rem_bt")
			  :g-up-bt (get-widget xmlc "kp/grouping_up_bt")
			  :g-down-bt (get-widget xmlc "kp/grouping_down_bt")
			  :cancel-bt (get-widget xmlc "kp/cancel_bt")
			  :print-bt (get-widget xmlc "kp/print_bt"))))
	(modify-bg (get-widget xmlc "kp/eventbox") 'normal *kc/dialog-title-eb-bg*)
	(when parent (set-transient-for (dialog kp-widget) parent))
	(kp/translate kp-widget)
	
	(kp/setup-g-treeview kp-widget)
	(set! (gui-callback? kp-widget) #f)
	(kp/fill-printers-combo kp-widget)
	(kp/fill-templates-combo kp-widget)

	(connect (dialog kp-widget)
		 'destroy-event
		 (lambda (widget event)
		   ;; don't do this if the dialog is launched using
		   ;; (run widget) modal 'style', it crashes
		   ;; (destroy widget)
		   (set! *kp-widget* #f)
		   #f))
	(connect (dialog kp-widget)
		 'delete-event
		 (lambda (widget event)
		   ;; same as destroy-event
		   (set! *kp-widget* #f)
		   #f))
	(connect (pdf-cb kp-widget)
		 'toggled
		 (lambda (widget)
		   (let ((value (get-active (pdf-cb kp-widget))))
		     (if value
			 (begin
			   (set-sensitive (printer-combo kp-widget) #f)
			   ;; (set-sensitive (latex-keep-files-cb kp-widget) #t)
			   (set! (pdf kp-widget) #t))
			 (begin
			   (set-sensitive (printer-combo kp-widget) #t)
			   ;; (set-sensitive (latex-keep-files-cb kp-widget) #f)
			   (set! (pdf kp-widget) #f))))))
	(connect (latex-keep-files-cb kp-widget)
		 'toggled
		 (lambda (widget)
		   (let ((value (get-active (latex-keep-files-cb kp-widget))))
		     (if value
			 (set! (tex kp-widget) #t)
			 (set! (tex kp-widget) #f)))))
	(connect (template-add-bt kp-widget)
		 'clicked
		 (lambda (widget)
		   ;; (format #t "Adding a template has been clicked ...~%")
		   (let* ((tpl-name (_ "change me"))
			  (tpl-id (db-pt/add-default tpl-name))
			  (dummy (kp/fill-templates-combo kp-widget 'reload))
			  (tpl-pos (gtk2/combo-find-row (template-combo kp-widget) tpl-name)))
		     ;; should not be necessary
		     ;; (set! (tpl-active-pos-at-entry-focus-in kp-widget) tpl-pos)
		     ;; (format #t "Tpl combo 'added: ~A, active: ~A~%" tpl-name tpl-pos)
		     (set-active (template-combo kp-widget) tpl-pos))))
	(connect (template-rem-bt kp-widget)
		 'clicked
		 (lambda (widget)
		   (kp/delete kp-widget)))
	(connect (template-combo kp-widget)
		 'changed ;; also called when the user changes the text entry
		 (lambda (combo)
		   (when (gui-callback? kp-widget)
		     (let ((active (get-active combo))
			   (value (get-text (template-entry kp-widget))))
		       ;; (format #t "'changed: ~A, tpl-pos: ~A~%" value active)
		       (case active
			 ((-1) 
			  ;; this occurs when value is not the same as
			  ;; the corresponding combo store entry: the
			  ;; user is modifying the name of the
			  ;; template, the combo's treeview is still
			  ;; filled with previous values and only will
			  ;; be updated on 'focus-out-event. In this
			  ;; specific case, we need to update the
			  ;; database and NOT calling
			  ;; kp/set-gtk-entries.
			  (kp/update kp-widget 'name value (tpl-active-pos-at-entry-focus-in kp-widget)))
			 (else
			  ;; we do not need to update the database, it
			  ;; is an active-pos change. We do need to
			  ;; call kp/set-gtk-entries.
			  (kp/set-gtk-entries kp-widget)))))))
	(connect (template-combo kp-widget)
		 'move-active
		 (lambda (combo scroll-type)
		   (kp/template-entry-focus-out-callback kp-widget 'move-active)))
	(connect (template-combo kp-widget)
		 'popup
		 (lambda (combo)
		   (kp/template-entry-focus-out-callback kp-widget 'popup)))
	(connect (template-entry kp-widget)
		 'focus-in-event
		 (lambda (entry event)
		   ;; (format #t "'focus-in-event called: ~S~%" event)
		   (set! (tpl-active-pos-at-entry-focus-in kp-widget) (get-active (template-combo kp-widget)))
		   ;; gtk2 requirement ...
		   #f))
	(connect (template-entry kp-widget)
		 'focus-out-event
		 (lambda (entry event)
		   ;; (format #t "'focus-out-event called: ~S~%" event)
		   (kp/template-entry-focus-out-callback kp-widget 'focus-out)))
	(connect (i-selected-rb kp-widget)
		 'toggled
		 (lambda (widget)
		   (set! (items kp-widget) 'selected)
		   (when (gui-callback? kp-widget) 
		     (kp/update kp-widget 'items "selected"))))
	(connect (i-all-rb kp-widget)
		 'toggled
		 (lambda (widget)
		   (set! (items kp-widget) 'all)
		   (when (gui-callback? kp-widget) 
		     (kp/update kp-widget 'items "all"))))
	(connect (m-draft-rb kp-widget)
		 'toggled
		 (lambda (widget)
		   ;; (format #t "Hey, been toggled or emitted :-~%")
		   (set! (mode kp-widget) 'draft)
		   (when (gui-callback? kp-widget)
		     (kp/update kp-widget 'mode "draft"))))	   
	(connect (m-com-rb kp-widget)
		 'toggled
		 (lambda (widget)
		   (set! (mode kp-widget) 'commercial)
		   (when (gui-callback? kp-widget)
		     (kp/update kp-widget 'mode "commercial"))))
	(connect (g-tv-sel kp-widget)
		 'changed ;; <- is called AFTER rows check/radio box 'toggled
		 (lambda (selection)
		   (kp/on-g-tv-row-change kp-widget) 
		   (receive (model iter)
		       (get-selected selection)
		     (if iter
			 (if (g-reselect-path? kp-widget)
			     (let ((tv-sel (g-tv-sel kp-widget))
				   (path (g-reselect-path? kp-widget)))
			       (unselect-all tv-sel)
			       (set! (g-reselect-path? kp-widget) #f)
			       (select-path tv-sel path)
			       ;; (format #t "Special changed - new-row:  ~S~%" (car path))
			       (kp/check-g-up-down-sensitive-needs kp-widget model iter (car path) 'special-changed))
			     (let* ((path (get-path model iter))
				    (row (car path))
				    (prev-gui-cb? (gui-callback? kp-widget)))
			       ;; (format #t "Changed - new-row:  ~S~%" row)
			       (kp/check-g-up-down-sensitive-needs kp-widget model iter (car path) 'changed)))))))
	(connect (g-up-bt kp-widget)
		 'clicked
		 (lambda (widget)
		   (kp/grouping-up kp-widget)))
	(connect (g-down-bt kp-widget)
		 'clicked
		 (lambda (widget)
		   (kp/grouping-down kp-widget)))
	(set-active (pdf-cb kp-widget) #t)
	(emit (pdf-cb kp-widget) 'toggled)
	(set-active (printer-combo kp-widget) 0)
	(set! (tpl-active-pos-at-entry-focus-in kp-widget) 0)
	(set-active (template-combo kp-widget) 0)
	(kp/set-gtk-entries kp-widget)
	(gtk2/set-sensitive `(,(latex-keep-files-cb kp-widget)
			      ,(i-selected-rb kp-widget)
			      ,(m-com-rb kp-widget)
			      ,(g-up-bt kp-widget) ;; no default row is select per default
			      ,(g-down-bt kp-widget))
			    #f)
	(gtk2/hide `(;; ,(get-widget xmlc "kp/templates_list_frame")
		     ,(g-add-bt kp-widget)
		     ,(g-rem-bt kp-widget)))
	(set! (gui-callback? kp-widget) #t)
	(kp/check-print-bt-sensitiveness kp-widget)
	(set! *kp-widget* kp-widget)
	kp-widget)))

(define (kp/get-core-ltx-field-specs kp-widget)
  (let* ((model (g-tv-model kp-widget))
	 (nb-rows (gtk-tree-model-iter-n-children model #f))
	 (print-only-field-specs (list)))
    (catch 'exit
	    (lambda ()
	      (dotimes (i nb-rows)
		(let* ((iter (get-iter model i))
		       (which (string->symbol (kpiter/get 'name model iter))))
		  (if (not (kpiter/get 'print model iter))
		      (throw 'exit i)
		      (unless (kpiter/get 'group model iter)
			(set! print-only-field-specs
			      (cons (db-pt/get-tex-field-spec which) print-only-field-specs)))))))
	    (lambda (key index)
	      index))
    (reverse! print-only-field-specs)))


;;;
;;; i18n - localisation
;;;

(define (kp/translate widget)
 #f)


#!

(use-modules (kise p-dialog))
(reload-module (resolve-module '(kise p-dialog)))
,m (kise p-dialog)

(kp/set-debug-variables)

;;;
;;; Test multiple imports - 1
;;;

(use-modules (kise tl-widget))
(use-modules (kise p-dialog))
(kp/make-dialog #f (string-append (aglobs/get 'glade-path) "/kise.glade"))
(define kp-widget $1)
(dialog kp-widget)


;;;
;;; Test multiple imports - 2
;;;

(use-modules (kise p-dialog))
(use-modules (kise tl-widget))
(kp/make-dialog #f (string-append (aglobs/get 'glade-path) "/kise.glade"))
(define kp-widget $1)
(dialog kp-widget)

!#

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


(define-module (foliot p-dialog)
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
  #:use-module (foliot db-foliot)
  #:use-module (foliot db-printing-templates)

  #:duplicates (merge-generics
		replace
		warn-override-core
		warn
		last)

  #:export (*fp-widget*
	    fp/make-dialog
	    <fp-widget>
	    fp/get-core-ltx-field-specs))


(g-export tpl-tuples
          dialog
          printer-combo
          pdf
          tex
          items
          mode
          template-combo)


(eval-when (expand load eval)
  (textdomain "p-dialog")
  (bindtextdomain "p-dialog" (ref %foliot-store 'pofdir)))


(define *foliot-p-dialog-offsets*
  '((print . 0)
    (group . 1)
    (name . 2)
    (asc . 3)
    (desc . 4)
    (none . 5)))

(define (fpiter/get-pos what)
  (assoc-ref *foliot-p-dialog-offsets* what))

(define (fpiter/get what model iter)
  (get-value model iter (fpiter/get-pos what)))

(define (fpiter/set what model iter value)
  ;; (format #t "offset: ~S~%" (cdr (assoc what offsets)))
  (set-value model iter (fpiter/get-pos what) value))


;;;
;;; Globals
;;;

(define *fp-widget* #f)

;; debug 'mode'
(define fp-widget #f)
(define model #f)
(define treeview #f)
(define selection #f)

(define (fp/set-debug-variables)
  (set! fp-widget *fp-widget*)
  (set! treeview (g-tv fp-widget))
  (set! model (g-tv-model fp-widget))
  (set! selection (g-tv-sel fp-widget)))


;;;
;;; Goops related
;;;

(define-class <fp-widget> ()
  (gui-callback? #:accessor gui-callback? #:init-keyword #:gui-callback? #:init-value #t)
  (tpl-tuples #:accessor tpl-tuples #:init-keyword #:tpl-tuples #:init-value #f)
  (xml-code #:accessor xml-code #:init-keyword #:xml-code #:init-value #f)
  (dialog #:accessor dialog #:init-keyword #:dialog #:init-value #f)
  (printer-frame-lb #:accessor printer-frame-lb #:init-keyword #:printer-frame-lb #:init-value #f)
  (printer-combo #:accessor printer-combo #:init-keyword #:printer-combo #:init-value #f)
  (pdf-cb #:accessor pdf-cb #:init-keyword #:pdf-cb #:init-value #f)
  (pdf #:accessor pdf #:init-keyword #:pdf #:init-value #f)
  (latex-keep-files-cb #:accessor latex-keep-files-cb #:init-keyword #:latex-keep-files-cb #:init-value #f)
  (tex #:accessor tex #:init-keyword #:tex #:init-value #f)
  (template-frame-lb #:accessor template-frame-lb #:init-keyword #:template-frame-lb #:init-value #f)
  (template-add-bt #:accessor template-add-bt #:init-keyword #:template-add-bt #:init-value #f)
  (template-rem-bt #:accessor template-rem-bt #:init-keyword #:template-rem-bt #:init-value #f)
  (template-name-lb #:accessor template-name-lb #:init-keyword #:template-name-lb #:init-value #f)
  (template-combo #:accessor template-combo #:init-keyword #:template-combo #:init-value #f)
  (template-entry #:accessor template-entry #:init-keyword #:template-entry #:init-value #f)
  (template-entry-focus-in-value #:accessor template-entry-focus-in-value #:init-value #f)
  (tpl-active-pos-at-entry-focus-in #:accessor tpl-active-pos-at-entry-focus-in #:init-value #f)
  (i-selected-rb #:accessor i-selected-rb #:init-keyword #:i-selected-rb #:init-value #f)
  (i-all-rb #:accessor i-all-rb #:init-keyword #:i-all-rb #:init-value #f)
  (items #:accessor items #:init-keyword #:items #:init-value #f)
  (m-draft-rb #:accessor m-draft-rb #:init-keyword #:m-draft-rb #:init-value #f)
  (m-com-rb #:accessor m-com-rb #:init-keyword #:m-com-rb #:init-value #f)
  (mode #:accessor mode #:init-keyword #:mode #:init-value #f)
  (g-tv #:accessor g-tv #:init-keyword #:g-tv #:init-value #f)
  (g-tv-model #:accessor g-tv-model #:init-keyword #:g-tv-model #:init-value #f)
  (g-tv-sel #:accessor g-tv-sel #:init-keyword #:g-tv-sel #:init-value #f)
  (grouping #:accessor grouping #:init-keyword #:grouping #:init-value #f)
  (g-add-bt #:accessor g-add-bt #:init-keyword #:g-add-bt #:init-value #f)
  (g-rem-bt #:accessor g-rem-bt #:init-keyword #:g-rem-bt #:init-value #f)
  (g-up-bt #:accessor g-up-bt #:init-keyword #:g-up-bt #:init-value #f)
  (g-down-bt #:accessor g-down-bt #:init-keyword #:g-down-bt #:init-value #f)
  (g-reselect-path? #:accessor g-reselect-path? #:init-value #f)
  (cancel-bt #:accessor cancel-bt #:init-keyword #:cancel-bt #:init-value #f)
  (print-bt #:accessor print-bt #:init-keyword #:print-bt #:init-value #f))


;;;
;;; Treeview related stuff
;;;

(define (fp/template-entry-focus-out-callback fp-widget . caller)
  ;; (format #t "focus-out-callback,~%  guicb?: ~S, caller: ~S~%" (gui-callback? fp-widget) caller)
  (when (gui-callback? fp-widget)
    (let* ((tpl-combo (template-combo fp-widget))
	   (active (get-active tpl-combo))
	   (value (get-text (template-entry fp-widget))))
      (if (= active -1)
	  (begin
	    (set! (gui-callback? fp-widget) #f)
	    (fp/update fp-widget 'name value (tpl-active-pos-at-entry-focus-in fp-widget))
	    (fp/fill-templates-combo fp-widget 'reload)
	    (let ((tpl-pos (gtk2/combo-find-row tpl-combo value)))
	      ;; (format #t "  new-value: ~a, new-pos: ~A~%" value tpl-pos)
	      (set-active tpl-combo tpl-pos)
	      (set! (tpl-active-pos-at-entry-focus-in fp-widget) tpl-pos))
	    (set! (gui-callback? fp-widget) #t))
	  ;; (format #t "  no update~%")
	  )))
  ;; gtk2 requirement ...
  #f)

(define (fp/on-g-tv-row-change fp-widget)
  ;; (format #t "fp/on-g-tv-row-change ...~%")
  (let* ((fp-dialog (dialog fp-widget))
	 (tpl-entry (template-entry fp-widget))
	 (focusw (get-focus fp-dialog)))
    (when (and focusw (eq? focusw tpl-entry))
      (fp/template-entry-focus-out-callback fp-widget))))

(define (fp/row-move fp-widget model iter toggle new-value
		     nb-rows first-grouped last-grouped last-printed)		     
  (let* ((tv-sel (g-tv-sel fp-widget))
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
	(fp/check-g-up-down-sensitive-needs fp-widget model (get-iter model new-pos) new-pos toggle)
	;; (set! (gui-callback? fp-widget) #f)
	(begin
	  (unselect-all tv-sel)
	  (if (< new-pos old-pos)
	      (move-before model
			   iter
			   (get-iter model new-path))
	      (move-after model
			  iter
			  (get-iter model new-path)))
	  (set! (g-reselect-path? fp-widget) new-path))
	;; (set! (gui-callback? fp-widget) #t)
	)))

(define (fp/check-print-bt-sensitiveness fp-widget)
  (let* ((model (g-tv-model fp-widget))
	 (nb-rows (gtk-tree-model-iter-n-children model #f))
	 (nb-print 0))
    (catch 'exit
      (lambda ()
	(dotimes (i nb-rows)
	  (let ((iter (get-iter model (list i))))
	    (if (fpiter/get 'print model iter)
		(set! nb-print (1+ nb-print))
		(throw 'exit #f)))))
      (lambda (key index) index))
    (if (> nb-print 0)
	(set-sensitive (print-bt fp-widget) #t)
	(set-sensitive (print-bt fp-widget) #f))))

(define (fp/printing-toggle-callback fp-widget model iter caller . value)
  ;; print ON
  ;;	-> does not affect other options
  ;;	-> row moved after the last row with print ON
  ;; print OFF
  ;; 	-> group OFF as well
  ;;	-> row moved after the last row with print ON [too]
  ;; grouping infos MUST be gathered BEFORE any setting on any toggle
  (receive (nb-rows first-grouped last-grouped last-printed)
      (if (gui-callback? fp-widget) (fp/get-grouping-infos fp-widget) (values -1 #f #f #f))
    (let* ((piter-get (lambda (model iter) (fpiter/get 'print model iter)))
	   (piter-set (lambda (model iter value) (fpiter/set 'print model iter value)))
	   (giter-get (lambda (model iter) (fpiter/get 'group model iter)))
	   (giter-set (lambda (model iter value) (fpiter/set 'group model iter value)))
	   (new-value (if (null? value)
			  (gtk2/fixed-toggled model iter piter-get piter-set)
			  (gtk2/fixed-toggled model iter piter-get piter-set (car value)))))
      (when (gui-callback? fp-widget)
	(fp/on-g-tv-row-change fp-widget) ;; <- making sure focus-out-event tpl-entry ...
	(fp/row-move fp-widget model iter 'print new-value
		     nb-rows first-grouped last-grouped last-printed)
	(unless new-value (gtk2/fixed-toggled model iter giter-get giter-set #f))
	(fp/check-print-bt-sensitiveness fp-widget)
	(fp/update-grouping fp-widget)))))

(define (fp/grouping-toggle-callback fp-widget model iter caller . value)
  ;; group ON
  ;;	-> print ON as well
  ;;	-> row moved after the last row with group ON
  ;; group OFF
  ;;	-> does not affect other options
  ;;	-> row moved after the last row with group ON
  ;; grouping infos MUST be gathered BEFORE any setting on any toggle

  (receive (nb-rows first-grouped last-grouped last-printed)
      (if (gui-callback? fp-widget) (fp/get-grouping-infos fp-widget) (values -1 #f #f #f))
    ;; (format #t "1st grouped: ~S, last: ~S, total: ~S~%" first-grouped last-grouped
    ;; (and first-grouped last-grouped (1+ (- last-grouped first-grouped))))
    (let ((piter-get (lambda (model iter) (fpiter/get 'print model iter)))
	  (piter-set (lambda (model iter value) (fpiter/set 'print model iter value)))
	  (giter-get (lambda (model iter) (fpiter/get 'group model iter)))
	  (giter-set (lambda (model iter value) (fpiter/set 'group model iter value))))
      (case caller
	((fill) ;; dialog creation, value is not null
	 (gtk2/fixed-toggled model iter giter-get giter-set (car value)))
	((toggled)
	 (let* ((old-value (giter-get model iter))
		(new-value (not old-value)))
	   (if new-value
	       ;; only if nb checked < 4
	       (if (or (not first-grouped)
		       (< (1+ (- last-grouped first-grouped)) 4))
		   (begin
		     (gtk2/fixed-toggled model iter giter-get giter-set)
		     (fp/on-g-tv-row-change fp-widget) ;; <- making sure focus-out-event tpl-entry ...
		     (fp/row-move fp-widget model iter 'group new-value
				  nb-rows first-grouped last-grouped last-printed)
		     (gtk2/fixed-toggled model iter piter-get piter-set #t)
		     (fp/check-print-bt-sensitiveness fp-widget)
		     (fp/update-grouping fp-widget))
		   (md1b/select-gui (dialog fp-widget)
				    (_ "Warning!")
				    (_ "Grouping:")
				    (_ "You may not select more then 4 items to be grouped.")
				    (lambda () 'nothing)
				    'dialog-warning))
	       (begin
		 (gtk2/fixed-toggled model iter giter-get giter-set)
		 (fp/on-g-tv-row-change fp-widget) ;; <- making sure focus-out-event tpl-entry ...
		 (fp/row-move fp-widget model iter 'group new-value
			      nb-rows first-grouped last-grouped last-printed)
		 (fp/update-grouping fp-widget)))))))))

(define (fp/sorting-radios-callback fp-widget model iter mode)
  (let* (;; (row (string->number path)) 
	 ;; (current-row tl-widget)) ;; <- this is not correct the
	 ;; user may click the checkbox of another iter AND this
	 ;; callback seems to be called before the row-changed one!
	 (asc-get (lambda (model iter) (fpiter/get 'asc model iter)))
	 (asc-set (lambda (model iter value) (fpiter/set 'asc model iter value)))
	 (desc-get (lambda (model iter) (fpiter/get 'desc model iter)))
	 (desc-set (lambda (model iter value) (fpiter/set 'desc model iter value)))
	 (none-get (lambda (model iter) (fpiter/get 'none model iter)))
	 (none-set (lambda (model iter value) (fpiter/set 'none model iter value))))
    ;; (format #t "sorting signal...~%")
    (case mode
      ((asc) (unless (asc-get model iter)
	       (let ((new-value (gtk2/fixed-toggled model iter asc-get asc-set)))
		 (gtk2/fixed-toggled model iter desc-get desc-set (not new-value))
		 (gtk2/fixed-toggled model iter none-get none-set (not new-value)))))
      ((desc) (unless (desc-get model iter)
		(let ((new-value (gtk2/fixed-toggled model iter desc-get desc-set)))
		  (gtk2/fixed-toggled model iter asc-get asc-set (not new-value))
		  (gtk2/fixed-toggled model iter none-get none-set (not new-value)))))
      ((none) (unless (none-get model iter)
		(let ((new-value (gtk2/fixed-toggled model iter none-get none-set)))
		  (gtk2/fixed-toggled model iter asc-get asc-set (not new-value))
		  (gtk2/fixed-toggled model iter desc-get desc-set (not new-value))))))
    (when (gui-callback? fp-widget)
      (fp/on-g-tv-row-change fp-widget)
      (fp/update-grouping fp-widget))))

(define (fp/add-g-model treeview)
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

(define (fp/add-g-columns fp-widget treeview)
  (let* ((model     (get-model treeview))
	 ;; GROUPING
	 (renderer1 (make <gtk-cell-renderer-toggle>))
	 (column1   (make <gtk-tree-view-column>
		      #:title       (_ "Grp.")
		      #:sizing      'fixed
		      #:fixed-width 40
		      #:expand      #f
		      #:alignment   .5))
	 ;; WHAT
	 (renderer2 (make <gtk-cell-renderer-text>))
	 (column2   (make <gtk-tree-view-column>
		      #:title       (_ "What")
		      #:sizing      'autosize
		      #:expand      #t
		      ;#:sizing      'grow-only
		      ;#:min-width   134
		      ;#:clickable   #f
		      ;#:resizable   #f
		      ;#:reorderable #f
		      #:alignment   .5))
	 ;; ASCENDING
	 (renderer3 (make <gtk-cell-renderer-toggle>
		      #:radio #t))
	 (column3   (make <gtk-tree-view-column>
		      #:title       (_ "Asc.")
		      #:sizing      'fixed
		      #:fixed-width 45
		      #:expand      #f
		      #:alignment   .5))
	 ;; DESCENDING
	 (renderer4 (make <gtk-cell-renderer-toggle>
		      #:radio #t))
	 (column4   (make <gtk-tree-view-column>
		      #:title       (_ "Desc.")
		      #:sizing      'fixed
		      #:fixed-width 45
		      #:expand      #f
		      #:alignment   .5))
	 ;; NONE
	 (renderer5 (make <gtk-cell-renderer-toggle>
		      #:radio #t))
	 (column5   (make <gtk-tree-view-column>
		      #:title       (_ "None")
		      #:sizing      'fixed
		      #:fixed-width 45
		      #:expand      #f
		      #:alignment   .5))
	 ;; PRINTING
	 (renderer6 (make <gtk-cell-renderer-toggle>))
	 (column6   (make <gtk-tree-view-column>
		      #:title       (_ "Print")
		      #:sizing      'fixed
		      #:fixed-width 45
		      #:expand      #f
		      #:alignment   .5))
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
		 (fp/printing-toggle-callback fp-widget model iter 'toggled))))
    (connect renderer1 ;; grouping
	     'toggled
	     (lambda (widget path)
	       ;; (format #t "group-toggle: ~S, path: ~S~%" widget path)
	       (let ((iter (get-iter model path)))
		 (fp/grouping-toggle-callback fp-widget model iter 'toggled))))
    (connect renderer3 ;; sorting, ascending
	     'toggled
	     (lambda (widget path)
	       (let ((iter (get-iter model path)))
		 (fp/sorting-radios-callback fp-widget model iter 'asc))))
    (connect renderer4 ;; sorting, descending
	     'toggled
	     (lambda (widget path)
	       (let ((iter (get-iter model path)))
		 (fp/sorting-radios-callback fp-widget model iter 'desc))))
    (connect renderer5 ;; sorting, none
	     'toggled
	     (lambda (widget path)
	       (let ((iter (get-iter model path)))
		 (fp/sorting-radios-callback fp-widget model iter 'none))))))

(define (fp/setup-g-treeview fp-widget)
  (let ((treeview (g-tv fp-widget)))
    (receive (model selection)
	(fp/add-g-model treeview)
      (set-mode selection 'single)
      (set! (g-tv-model fp-widget) model)
      (set! (g-tv-sel fp-widget) selection))
    (fp/add-g-columns fp-widget treeview)
    fp-widget))

(define (fp/fill-g-treeview fp-widget items)
  ;; Note: whenever the structure of db-pt/default-fields changes,
  ;;       this needs to be edited too
  (let ((model (g-tv-model fp-widget)))
    (gtk-list-store-clear model)
    (for-each (lambda (item)
		(let* ((iter (gtk-list-store-append model))
		       (print? (db-pt/df-get 'print item))
		       (group? (db-pt/df-get 'group item))
		       (name (db-pt/df-get 'name item))
		       (sort-mode (db-pt/df-get 'sort item)))
		  (fp/printing-toggle-callback fp-widget model iter 'fill print?)
		  (fp/grouping-toggle-callback fp-widget model iter 'fill group?)
		  (fpiter/set 'name model iter name)
		  (case sort-mode
		    ((asc) (fp/sorting-radios-callback fp-widget model iter 'asc))
		    ((desc) (fp/sorting-radios-callback fp-widget model iter 'desc))
		    ((none) (fp/sorting-radios-callback fp-widget model iter 'none))
		    (else
		     (format #t "Unkown sorting mode ~S~%" sort-mode)))))
	items)))

(define (fp/check-template-toolbar-buttons-sensitivity fp-widget)
  (let ((tpl-tuple-nb (length (tpl-tuples fp-widget))))
    (case (length (tpl-tuples fp-widget))
      ((0 1) (set-sensitive (template-rem-bt fp-widget) #f))
      (else
       (set-sensitive (template-rem-bt fp-widget) #t)))))

(define (fp/set-gtk-entries fp-widget)
  (let* ((tpl-pos (get-active (template-combo fp-widget)))
	 (tpl-tuple (db-pt/get-tuple (tpl-tuples fp-widget) tpl-pos))
	 (tpl-items (db-pt/get tpl-tuple 'items))
	 (tpl-mode (db-pt/get tpl-tuple 'mode))
	 (tpl-grouping-str (db-pt/get tpl-tuple 'group_and_sort))
	 (tpl-grouping (with-input-from-string tpl-grouping-str read))
	 (prev-gui-cb? (gui-callback? fp-widget)))
    ;; (format #t "fp/set-gtk-entries, tpl-pos ~S~%tuple: ~S~%" tpl-pos tpl-tuple)
    (set! (gui-callback? fp-widget) #f)
    (fp/check-template-toolbar-buttons-sensitivity fp-widget)
    (case (string->symbol tpl-items)
      ((selected)
       (set-active (i-selected-rb fp-widget) #t)
       (emit (i-selected-rb fp-widget) 'toggled))
      ((all) 
       (set-active (i-all-rb fp-widget) #t)
       (emit (i-all-rb fp-widget) 'toggled)))
    (case (string->symbol tpl-mode)
      ((draft)
       (set-active (m-draft-rb fp-widget) #t)
       (emit (m-draft-rb fp-widget) 'toggled))
      ((commercial)
       (set-active (m-com-rb fp-widget) #t)
       (emit (m-com-rb fp-widget) 'toggled)))
    (fp/fill-g-treeview fp-widget tpl-grouping)
    (set! (tpl-active-pos-at-entry-focus-in fp-widget) tpl-pos)
    (set! (gui-callback? fp-widget) prev-gui-cb?)))


;;;
;;; Printers
;;;

(define (fp/fill-printers-combo fp-widget)
  #t)


;;;
;;; Templates
;;;

(define (fp/get-templates)
  (let ((tuples (db-pt/select-all)))
    (if (null? tuples)
	(begin
	  (db-pt/add-default)
	  (db-pt/select-all))
	tuples)))

(define (fp/get-template-names fp-widget)
  (let ((values (list)))
    (for-each (lambda (tpl-tuple)
		(set! values (cons (db-pt/get tpl-tuple 'name) values)))
	(tpl-tuples fp-widget))
    (reverse! values)))

(define (fp/fill-templates-combo fp-widget . reload?)
  (let ((prev-gui-cb? (gui-callback? fp-widget)))
    (when (not (null? reload?))
      (set! (tpl-tuples fp-widget) (fp/get-templates)))
    (set! (gui-callback? fp-widget) #f)
    (gtk2/fill-combo (template-combo fp-widget) (fp/get-template-names fp-widget))
    (set! (gui-callback? fp-widget) prev-gui-cb?)))

(define (fp/update fp-widget what value . row)
  (let* ((tpl-pos (if (null? row) (get-active (template-combo fp-widget)) (car row)))
	 (tpl-tuple (db-pt/get-tuple (tpl-tuples fp-widget) tpl-pos)))
    ;; (format #t "fp/update: tpl-pos ~A~%  before: ~S~%" tpl-pos tpl-tuple)
    (db-pt/update tpl-tuple what value)
    #;(format #t "  after: ~S~%" tpl-tuple)))

(define (fp/build-grouping-value fp-widget)
  (let* ((model (g-tv-model fp-widget))
	 (nb-rows (gtk-tree-model-iter-n-children model #f))
	 (value (list)))
    (dotimes (i nb-rows)
      (let* ((iter (get-iter model (list i)))
	     (print? (fpiter/get 'print model iter))
	     (group? (fpiter/get 'group model iter))
	     (name (fpiter/get 'name model iter))
	     (asc? (fpiter/get 'asc model iter))
	     (desc? (fpiter/get 'desc model iter))
	     (none? (fpiter/get 'none model iter)))
	;; (format #t "Row: ~A, ter: ~S~%" i iter)
	(set! value
	      (cons (list print? group? name
			  (cond (asc? 'asc) (desc? 'desc) (none? 'none)))
		    value))))
    (reverse! value)))

(define (fp/update-grouping fp-widget)
  (let ((value (fp/build-grouping-value fp-widget)))
    ;; (format #t "Grouping: ~S~%" value)
    (fp/update fp-widget 'group_and_sort (format #f "~S" value))))


;;;
;;; Grouping and sorting treeview up and down callbacks
;;;

(define (fp/grouping-up fp-widget)
  ;; (format #t "Grouping up has been clicked~%")
  (let ((tv-sel (g-tv-sel fp-widget)))
    (receive (model iter)
	(get-selected tv-sel)
      (let* ((old-pos (car (get-path model iter)))
	     (new-pos (1- old-pos))
	     (new-path (list new-pos)))
	(move-before model
		     iter
		     (get-iter model new-path))
	(set! (gui-callback? fp-widget) #f)
	(unselect-all tv-sel)
	(select-path tv-sel new-path)
	(fp/update-grouping fp-widget)
	(fp/check-g-up-down-sensitive-needs fp-widget model (get-iter model new-pos) new-pos)
	(set! (gui-callback? fp-widget) #t)))))

(define (fp/grouping-down fp-widget)
  ;; (format #t "Grouping up has been clicked~%")
  (let ((tv-sel (g-tv-sel fp-widget)))
    (receive (model iter)
	(get-selected tv-sel)
      (let* ((old-pos (car (get-path model iter)))
	     (new-pos (1+ old-pos))
	     (new-path (list new-pos)))
	(move-after model
		    iter
		    (get-iter model new-path))
	(set! (gui-callback? fp-widget) #f)
	(unselect-all tv-sel)
	(select-path tv-sel new-path)
	(fp/update-grouping fp-widget)
	(fp/check-g-up-down-sensitive-needs fp-widget model (get-iter model new-pos) new-pos)
	(set! (gui-callback? fp-widget) #t)))))

(define (fp/get-grouping-infos fp-widget)
  ;; if grouped items, per definition the first is at pos 0, and only
  ;; in this case do we search the pos of the last, starting from the
  ;; bottom.
  (let* ((model (g-tv-model fp-widget))
	 (nb-rows (gtk-tree-model-iter-n-children model #f))
	 (first-grouped (if (fpiter/get 'group model (get-iter model 0)) 0 #f))
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
		  ;; 	  i (fpiter/get 'print model iter) (fpiter/get 'group model iter))
		  (unless last-printed
		    (if (fpiter/get 'print model iter) (set! last-printed i)))
		  (if (fpiter/get 'group model iter)
		      (throw 'exit i)))))
	    (lambda (key index) index)))
    (values nb-rows first-grouped last-grouped last-printed)))

(define (fp/display-get-grouping-debug-infos nb-rows first-grouped last-grouped last-printed caller)
  (format #t "Nb-Rows: ~A, First grouped: ~S, Last grouped: ~S, Last printed: ~S, Caller: ~S~%"
	  nb-rows first-grouped last-grouped last-printed caller))

(define (fp/check-g-up-down-sensitive-needs fp-widget model iter row . caller)
  (let ((up-bt (g-up-bt fp-widget))
	(down-bt (g-down-bt fp-widget)))
    (receive (nb-rows first-grouped last-grouped last-printed)
	(fp/get-grouping-infos fp-widget)
      ;; (fp/display-get-grouping-debug-infos nb-rows first-grouped last-grouped last-printed caller)
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

(define (fp/delete-msg-str)
  ;; with "~10,,,' @A": see comment in tl-widget.scm
  (string-append (_ "Are you sure you want to delete this template ?")
		 "~%~%	~10,,,' A: ~A"))

(define (fp/delete fp-widget)
  (let* ((tpl-pos (get-active (template-combo fp-widget)))
	 (tpl-tuple (db-pt/get-tuple (tpl-tuples fp-widget) tpl-pos))
	 (tpl-id (db-pt/get tpl-tuple 'id))
	 (tpl-name (db-pt/get tpl-tuple 'name)))
    (md2b/select-gui (dialog fp-widget)
		     (_ "Confirm dialog")
		     (_ "Deletion")
		     (format #f "~?" (fp/delete-msg-str)
			     (list (_ "Name") tpl-name))
		     (lambda ()
		       (db-pt/delete tpl-id)
		       (let* ((dummy (fp/fill-templates-combo fp-widget 'reload))
			      (tpl-nb (length (tpl-tuples fp-widget))))
			 (if (>= tpl-pos (1- tpl-nb))
			     (set-active (template-combo fp-widget) (1- tpl-nb))
			     (set-active (template-combo fp-widget) tpl-pos))))
		     (lambda () 'nothing))))


;;;
;;; Making the dialog
;;;

(define (fp/make-dialog parent glade-f)
  (if *fp-widget*
      ;; need to refresh the templates combo: the user may have connected to
      ;; another database.
      (let ((fp-widget *fp-widget*))
	(set! (gui-callback? fp-widget) #f)
	(fp/fill-templates-combo fp-widget 'reload)
	(set! (gui-callback? fp-widget) #t)
	fp-widget)
      (let* ((xmlc (glade-xml-new glade-f #f "fp/dialog"))
	     (fp-widget (make <fp-widget>
			  #:tpl-tuples (fp/get-templates)
			  #:xml-code xmlc
			  #:dialog (get-widget xmlc "fp/dialog")
			  #:printer-frame-lb (get-widget xmlc "fp/printer_frame_lb")
			  #:printer-combo (get-widget xmlc "fp/printer_combo")
			  #:pdf-cb (get-widget xmlc "fp/pdf_cb")
			  #:latex-keep-files-cb (get-widget xmlc "fp/latex_keep_files_cb")
			  #:template-add-bt (get-widget xmlc "fp/template_add_bt")
			  #:template-rem-bt (get-widget xmlc "fp/template_rem_bt")
			  #:template-combo (get-widget xmlc "fp/template_combo")
			  #:template-entry (gtk-bin-get-child (get-widget xmlc "fp/template_combo"))
			  #:i-selected-rb (get-widget xmlc "fp/items_entry_rb")
			  #:i-all-rb (get-widget xmlc "fp/items_all_rb")
			  #:m-draft-rb (get-widget xmlc "fp/mode_draft_rb")
			  #:m-com-rb (get-widget xmlc "fp/mode_commercial_rb")
			  #:g-tv (get-widget xmlc "fp/grouping_tv")
			  #:g-add-bt (get-widget xmlc "fp/grouping_add_bt")
			  #:g-rem-bt (get-widget xmlc "fp/grouping_rem_bt")
			  #:g-up-bt (get-widget xmlc "fp/grouping_up_bt")
			  #:g-down-bt (get-widget xmlc "fp/grouping_down_bt")
			  #:cancel-bt (get-widget xmlc "fp/cancel_bt")
			  #:print-bt (get-widget xmlc "fp/print_bt"))))
	(modify-bg (get-widget xmlc "fp/eventbox") 'normal *dialog-title-eb-bg*)
	(when parent (set-transient-for (dialog fp-widget) parent))
	(fp/translate fp-widget)
	
	(fp/setup-g-treeview fp-widget)
	(set! (gui-callback? fp-widget) #f)
	(fp/fill-printers-combo fp-widget)
	(fp/fill-templates-combo fp-widget)

	(connect (dialog fp-widget)
		 'destroy-event
		 (lambda (widget event)
		   ;; don't do this if the dialog is launched using
		   ;; (run widget) modal 'style', it crashes
		   ;; (destroy widget)
		   (set! *fp-widget* #f)
		   #f))
	(connect (dialog fp-widget)
		 'delete-event
		 (lambda (widget event)
		   ;; same as destroy-event
		   (set! *fp-widget* #f)
		   #f))
	(connect (pdf-cb fp-widget)
		 'toggled
		 (lambda (widget)
		   (let ((value (get-active (pdf-cb fp-widget))))
		     (if value
			 (begin
			   (set-sensitive (printer-combo fp-widget) #f)
			   ;; (set-sensitive (latex-keep-files-cb fp-widget) #t)
			   (set! (pdf fp-widget) #t))
			 (begin
			   (set-sensitive (printer-combo fp-widget) #t)
			   ;; (set-sensitive (latex-keep-files-cb fp-widget) #f)
			   (set! (pdf fp-widget) #f))))))
	(connect (latex-keep-files-cb fp-widget)
		 'toggled
		 (lambda (widget)
		   (let ((value (get-active (latex-keep-files-cb fp-widget))))
		     (if value
			 (set! (tex fp-widget) #t)
			 (set! (tex fp-widget) #f)))))
	(connect (template-add-bt fp-widget)
		 'clicked
		 (lambda (widget)
		   ;; (format #t "Adding a template has been clicked ...~%")
		   (let* ((tpl-name (_ "change me"))
			  (tpl-id (db-pt/add-default tpl-name))
			  (dummy (fp/fill-templates-combo fp-widget 'reload))
			  (tpl-pos (gtk2/combo-find-row (template-combo fp-widget) tpl-name)))
		     ;; should not be necessary
		     ;; (set! (tpl-active-pos-at-entry-focus-in fp-widget) tpl-pos)
		     ;; (format #t "Tpl combo 'added: ~A, active: ~A~%" tpl-name tpl-pos)
		     (set-active (template-combo fp-widget) tpl-pos))))
	(connect (template-rem-bt fp-widget)
		 'clicked
		 (lambda (widget)
		   (fp/delete fp-widget)))
	(connect (template-combo fp-widget)
		 'changed ;; also called when the user changes the text entry
		 (lambda (combo)
		   (when (gui-callback? fp-widget)
		     (let ((active (get-active combo))
			   (value (get-text (template-entry fp-widget))))
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
			  ;; fp/set-gtk-entries.
			  (fp/update fp-widget 'name value (tpl-active-pos-at-entry-focus-in fp-widget)))
			 (else
			  ;; we do not need to update the database, it
			  ;; is an active-pos change. We do need to
			  ;; call fp/set-gtk-entries.
			  (fp/set-gtk-entries fp-widget)))))))
	(connect (template-combo fp-widget)
		 'move-active
		 (lambda (combo scroll-type)
		   (fp/template-entry-focus-out-callback fp-widget 'move-active)))
	(connect (template-combo fp-widget)
		 'popup
		 (lambda (combo)
		   (fp/template-entry-focus-out-callback fp-widget 'popup)))
	(connect (template-entry fp-widget)
		 'focus-in-event
		 (lambda (entry event)
		   ;; (format #t "'focus-in-event called: ~S~%" event)
		   (set! (tpl-active-pos-at-entry-focus-in fp-widget) (get-active (template-combo fp-widget)))
		   ;; gtk2 requirement ...
		   #f))
	(connect (template-entry fp-widget)
		 'focus-out-event
		 (lambda (entry event)
		   ;; (format #t "'focus-out-event called: ~S~%" event)
		   (fp/template-entry-focus-out-callback fp-widget 'focus-out)))
	(connect (i-selected-rb fp-widget)
		 'toggled
		 (lambda (widget)
		   (set! (items fp-widget) 'selected)
		   (when (gui-callback? fp-widget) 
		     (fp/update fp-widget 'items "selected"))))
	(connect (i-all-rb fp-widget)
		 'toggled
		 (lambda (widget)
		   (set! (items fp-widget) 'all)
		   (when (gui-callback? fp-widget) 
		     (fp/update fp-widget 'items "all"))))
	(connect (m-draft-rb fp-widget)
		 'toggled
		 (lambda (widget)
		   ;; (format #t "Hey, been toggled or emitted :-~%")
		   (set! (mode fp-widget) 'draft)
		   (when (gui-callback? fp-widget)
		     (fp/update fp-widget 'mode "draft"))))	   
	(connect (m-com-rb fp-widget)
		 'toggled
		 (lambda (widget)
		   (set! (mode fp-widget) 'commercial)
		   (when (gui-callback? fp-widget)
		     (fp/update fp-widget 'mode "commercial"))))
	(connect (g-tv-sel fp-widget)
		 'changed ;; <- is called AFTER rows check/radio box 'toggled
		 (lambda (selection)
		   (fp/on-g-tv-row-change fp-widget) 
		   (receive (model iter)
		       (get-selected selection)
		     (if iter
			 (if (g-reselect-path? fp-widget)
			     (let ((tv-sel (g-tv-sel fp-widget))
				   (path (g-reselect-path? fp-widget)))
			       (unselect-all tv-sel)
			       (set! (g-reselect-path? fp-widget) #f)
			       (select-path tv-sel path)
			       ;; (format #t "Special changed - new-row:  ~S~%" (car path))
			       (fp/check-g-up-down-sensitive-needs fp-widget model iter (car path) 'special-changed))
			     (let* ((path (get-path model iter))
				    (row (car path))
				    (prev-gui-cb? (gui-callback? fp-widget)))
			       ;; (format #t "Changed - new-row:  ~S~%" row)
			       (fp/check-g-up-down-sensitive-needs fp-widget model iter (car path) 'changed)))))))
	(connect (g-up-bt fp-widget)
		 'clicked
		 (lambda (widget)
		   (fp/grouping-up fp-widget)))
	(connect (g-down-bt fp-widget)
		 'clicked
		 (lambda (widget)
		   (fp/grouping-down fp-widget)))
	(set-active (pdf-cb fp-widget) #t)
	(emit (pdf-cb fp-widget) 'toggled)
	(set-active (printer-combo fp-widget) 0)
	(set! (tpl-active-pos-at-entry-focus-in fp-widget) 0)
	(set-active (template-combo fp-widget) 0)
	(fp/set-gtk-entries fp-widget)
	(gtk2/set-sensitive `(,(latex-keep-files-cb fp-widget)
			      ,(i-selected-rb fp-widget)
			      ,(m-com-rb fp-widget)
			      ,(g-up-bt fp-widget) ;; no default row is select per default
			      ,(g-down-bt fp-widget))
			    #f)
	(gtk2/hide `(;; ,(get-widget xmlc "fp/templates_list_frame")
		     ,(g-add-bt fp-widget)
		     ,(g-rem-bt fp-widget)))
	(set! (gui-callback? fp-widget) #t)
	(fp/check-print-bt-sensitiveness fp-widget)
	(set! *fp-widget* fp-widget)
	fp-widget)))

(define (fp/get-core-ltx-field-specs fp-widget)
  (let* ((model (g-tv-model fp-widget))
	 (nb-rows (gtk-tree-model-iter-n-children model #f))
	 (print-only-field-specs (list)))
    (catch 'exit
	    (lambda ()
	      (dotimes (i nb-rows)
		(let* ((iter (get-iter model i))
		       (which (string->symbol (fpiter/get 'name model iter))))
		  (if (not (fpiter/get 'print model iter))
		      (throw 'exit i)
		      (unless (fpiter/get 'group model iter)
			(set! print-only-field-specs
			      (cons (db-pt/get-tex-field-spec which) print-only-field-specs)))))))
	    (lambda (key index)
	      index))
    (reverse! print-only-field-specs)))


;;;
;;; i18n - localisation
;;;

(define (fp/translate widget)
  (let ((xmlc (xml-code widget)))
    (set-title (dialog widget) (_ "Printing"))
    (set-markup (get-widget xmlc "fp/title_lb")
		(format #f "<span foreground=\"darkblue\" size=\"x-large\"><b>~A</b></span>~%<b>~A</b>"
			(_ "GNU Foliot printing dialog")
			(_ "Add, remove, update templates and print.")))))


#!

(fp/set-debug-variables)

;;;
;;; Test multiple imports - 1
;;;

(use-modules (foliot tl-widget))
(use-modules (foliot p-dialog))
(fp/make-dialog #f (string-append (ref %foliot-store 'glade-path) "/foliot.glade"))
(define fp-widget $1)
(dialog fp-widget)


;;;
;;; Test multiple imports - 2
;;;

(use-modules (foliot p-dialog))
(use-modules (foliot tl-widget))
(fp/make-dialog #f (string-append (ref %foliot-store 'glade-path) "/foliot.glade"))
(define fp-widget $1)
(dialog fp-widget)

!#

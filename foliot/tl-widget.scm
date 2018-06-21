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


(define-module (foliot tl-widget)
  #:use-module (ice-9 format)
  #:use-module (ice-9 receive)
  #:use-module (oop goops)
  #:use-module (gnome gobject)
  #:use-module (gnome gtk)
  #:use-module (gnome gtk gdk-event)
  #:use-module (gnome glade)
  #:use-module (grip module)
  #:use-module (grip queue)
  #:use-module (grip iter)
  #:use-module (grip date)
  #:use-module (grip i18n)
  #:use-module (grip utils)
  #:use-module (grip string)
  #:use-module (grip number)
  #:use-module (grip gnome)
  #:use-module (grip sqlite)
  #:use-module (grip db filter)
  #:use-module (foliot config)
  #:use-module (foliot globals)
  #:use-module (foliot colours)
  #:use-module (foliot db)
  #:use-module (foliot iter)
  ;; #:use-module (foliot what-tree)
  
  #:duplicates (merge-generics
		replace
		warn-override-core
		warn
		last)

  #:export (<foliot/tl-widget>

	    ftlw/write-config

	    ftlw/create-db-checks
	    ftlw/open-db-checks
	    ftlw/open-db
	    ftlw/fill-combos
	    ftlw/connect-combos

	    ftlw/get-tuple
	    ftlw/get
	    ftlw/set
	    ftlw/setup-treeview
	    ftlw/fill-tv
	    ftlw/get-hr-hrs
	    ftlw/get-hour-hours
	    ftlw/get-day-days
	    ftlw/get-totals
	    ftlw/update-status-bar-1
	    ftlw/select-ctime
	    ftlw/update-totals-status-bars

	    ftlw/set-cur-globals
	    ftlw/select-row
	    ftlw/update-store-check-position
	    ftlw/entry-std-cb
	    ftlw/make-tl-widget

	    ftlw/add-id ;; filter additional 'or' id set
	    ftlw/del-id
	    ftlw/import
	    ftlw/add
	    ftlw/duplicate
	    ftlw/delete

	    ftlw/check-nav-tb-sensitive-needs
	    ftlw/empty-subset-or-db-mode
	    ftlw/no-db-mode
	    ftlw/normal-mode
	    ftlw/filter-apply
	    ftlw/filter-clear))

(g-export gui-callback?
          user-name

          db-file
          db-tuples
          whos
          for-whoms
          whats
          current-row
          current-iter

          glade-file
          xml-code
          dialog
          menubar

          ;; sorting-lb
          ;; sorting-combo

          con-bt
          import-bt
          quit-bt
          dup-bt
          add-bt
          del-bt
          print-bt

          first-bt
          prev-bt
          next-bt
          last-bt
          ;; help-bt
          prefs-bt

          reference-lb
          reference-entry
          reference-eb

          date-lb
          date-entry
          date-icon   ;; exported because currently hidden in foliot.scm
          date-edit   ;; exported because currently hidden in foliot.scm

          who-lb
          who-entry
          who-combo

          for-whom-lb
          for-whom-entry
          for-whom-combo

          duration-lb
          duration-sb

          what-lb
          what-combo
          what-entry
          ;; what-tv
          ;; what-tv-model
          ;; what-tv-sel

          to-be-charged-cb

          description-lb
          description-sw
          description-entry

          db-name-lb1
          db-name-lb2
          db-name-lb3

          filter-icon ;; exported because currently hidden in foliot.scm
          filter-apply-bt
          filter-clear-bt
          filter-select-bt

          filter-date-entry
          filter-who-entry
          filter-who-lb
          filter-for-whom-entry
          filter-for-whom-lb
          filter-what-entry
          filter-what-lb
          filter-description-entry
          filter-to-be-charged-lb
          filter-to-be-charged-combo

          active-filter
          id-set

          sw
          tv
          tv-model
          tv-sel
          g-reselect-path?

          status-bar-1   ;; records info
          status-bar-2   ;; app messages
          status-bar-3   ;; total time
          status-bar-4   ;; charged time
          show-me)


(eval-when (expand load eval)
  (re-export-public-interface (oop goops)
			      (gnome gobject)
			      (gnome gtk)
			      (gnome glade)
			      (foliot config))
  (textdomain "tl-widget")
  (bindtextdomain "tl-widget" (ref %foliot-store 'pofdir)))


;;;
;;; <foliot/tl-widget>
;;;

(define-class <foliot/tl-widget> () ;; (<foliot/config>)
  (user-name #:accessor user-name #:init-keyword #:user-name #:init-value #f)
  (gui-callback? #:accessor gui-callback? #:init-keyword #:gui-callback? #:init-value #t)

  (db-file #:accessor db-file #:init-keyword #:db-file #:init-value #f)
  (db-tuples #:accessor db-tuples #:init-keyword #:db-tuples #:init-value #f)
  (whos #:accessor whos #:init-keyword #:whos #:init-value #f)
  (for-whoms #:accessor for-whoms #:init-keyword #:for-whoms #:init-value #f)
  (whats #:accessor whats #:init-keyword #:whats #:init-value #f)
  (current-row #:accessor current-row #:init-keyword #:current-row #:init-value #f)
  (current-iter #:accessor current-iter #:init-keyword #:current-iter #:init-value #f)
  (glade-file #:accessor glade-file #:init-keyword #:glade-file #:init-value #f)
  (xml-code #:accessor xml-code #:init-keyword #:xml-code #:init-value #f)

  (dialog #:accessor dialog #:init-keyword #:dialog #:init-value #f)
  (menubar #:accessor menubar #:init-keyword #:menubar #:init-value #f)
  (tooltip #:accessor tooltip #:init-keyword #:tooltip #:init-value #f)

  (sorting-lb #:accessor sorting-lb #:init-keyword #:sorting-lb #:init-value #f)
  (sorting-combo #:accessor sorting-combo #:init-keyword #:sorting-combo #:init-value #f)

  (con-bt #:accessor con-bt #:init-keyword #:con-bt #:init-value #f)
  (import-bt #:accessor import-bt #:init-keyword #:import-bt #:init-value #f)
  (quit-bt #:accessor quit-bt #:init-keyword #:quit-bt #:init-value #f)
  (dup-bt #:accessor dup-bt #:init-keyword #:dup-bt #:init-value #f)
  (add-bt #:accessor add-bt #:init-keyword #:add-bt #:init-value #f)
  (del-bt #:accessor del-bt #:init-keyword #:del-bt #:init-value #f)
  (print-bt #:accessor print-bt #:init-keyword #:print-bt #:init-value #f)

  (first-bt #:accessor first-bt #:init-keyword #:first-bt #:init-value #f)
  (prev-bt #:accessor prev-bt #:init-keyword #:prev-bt #:init-value #f)
  (next-bt #:accessor next-bt #:init-keyword #:next-bt #:init-value #f)
  (last-bt #:accessor last-bt #:init-keyword #:last-bt #:init-value #f)
  (help-bt #:accessor help-bt #:init-keyword #:help-bt #:init-value #f)
  (prefs-bt #:accessor prefs-bt #:init-keyword #:prefs-bt #:init-value #f)

  (reference-lb #:accessor reference-lb #:init-keyword #:reference-lb #:init-value #f)
  (reference-entry #:accessor reference-entry #:init-keyword #:reference-entry #:init-value #f)
  (reference-eb #:accessor reference-eb #:init-keyword #:reference-eb #:init-value #f)
  (date-lb #:accessor date-lb #:init-keyword #:date-lb #:init-value #f)
  (date-entry #:accessor date-entry #:init-keyword #:date-entry #:init-value #f)
  (date-icon #:accessor date-icon #:init-keyword #:date-icon #:init-value #f)
  (date-edit #:accessor date-edit #:init-keyword #:date-edit #:init-value #f)
  (who-lb #:accessor who-lb #:init-keyword #:who-lb #:init-value #f)
  (who-entry #:accessor who-entry #:init-keyword #:who-entry #:init-value #f)
  (who-combo #:accessor who-combo #:init-keyword #:who-combo #:init-value #f)
  (for-whom-lb #:accessor for-whom-lb #:init-keyword #:for-whom-lb #:init-value #f)
  (for-whom-entry #:accessor for-whom-entry #:init-keyword #:for-whom-entry #:init-value #f)
  (for-whom-combo #:accessor for-whom-combo #:init-keyword #:for-whom-combo #:init-value #f)
  (duration-lb #:accessor duration-lb #:init-keyword #:duration-lb #:init-value #f)
  (duration-sb #:accessor duration-sb #:init-keyword #:duration-sb #:init-value #f)
  (what-lb #:accessor what-lb #:init-keyword #:what-lb #:init-value #f)
  (what-combo #:accessor what-combo #:init-keyword #:what-combo #:init-value #f)
  (what-entry #:accessor what-entry #:init-keyword #:what-entry #:init-value #f)
  ;; (what-tv #:accessor what-tv #:init-keyword #:what-tv #:init-value #f)
  ;; (what-tv-model #:accessor what-tv-model #:init-keyword #:what-tv-model #:init-value #f)
  ;; (what-tv-sel #:accessor what-tv-sel #:init-keyword #:what-tv-sel #:init-value #f)
  (to-be-charged-cb #:accessor to-be-charged-cb #:init-keyword #:to-be-charged-cb #:init-value #f)
  (description-lb #:accessor description-lb #:init-keyword #:description-lb #:init-value #f)
  (description-sw #:accessor description-sw #:init-keyword #:description-sw #:init-value #f)
  (description-entry #:accessor description-entry #:init-keyword #:description-entry #:init-value #f)

  (db-name-lb1 #:accessor db-name-lb1 #:init-keyword #:db-name-lb1 #:init-value #f)
  (db-name-lb2 #:accessor db-name-lb2 #:init-keyword #:db-name-lb2 #:init-value #f)
  (db-name-lb3 #:accessor db-name-lb3 #:init-keyword #:db-name-lb3 #:init-value #f)

  (filter-icon #:accessor filter-icon #:init-keyword #:filter-icon #:init-value #f)
  (filter-criteria-lb #:accessor filter-criteria-lb #:init-keyword #:filter-criteria-lb #:init-value #f)
  (filter-apply-bt #:accessor filter-apply-bt #:init-keyword #:filter-apply-bt #:init-value #f)
  (filter-clear-bt #:accessor filter-clear-bt #:init-keyword #:filter-clear-bt #:init-value #f)
  (filter-select-bt #:accessor filter-select-bt #:init-keyword #:filter-select-bt #:init-value #f)

  (filter-date-lb #:accessor filter-date-lb #:init-keyword #:filter-date-lb #:init-value #f)
  (filter-date-entry #:accessor filter-date-entry #:init-keyword #:filter-date-entry #:init-value #f)
  (filter-who-lb #:accessor filter-who-lb #:init-keyword #:filter-who-lb #:init-value #f)
  (filter-who-entry #:accessor filter-who-entry #:init-keyword #:filter-who-entry #:init-value #f)
  (filter-for-whom-lb #:accessor filter-for-whom-lb #:init-keyword #:filter-for-whom-lb #:init-value #f)
  (filter-for-whom-entry #:accessor filter-for-whom-entry #:init-keyword #:filter-for-whom-entry #:init-value #f)
  (filter-what-lb #:accessor filter-what-lb #:init-keyword #:filter-what-lb #:init-value #f)
  (filter-what-entry #:accessor filter-what-entry #:init-keyword #:filter-what-entry #:init-value #f)
  (filter-description-lb #:accessor filter-description-lb #:init-keyword #:filter-description-lb #:init-value #f)
  (filter-description-entry #:accessor filter-description-entry #:init-keyword #:filter-description-entry #:init-value #f)
  (filter-to-be-charged-lb #:accessor filter-to-be-charged-lb #:init-keyword #:filter-to-be-charged-lb #:init-value #f)
  (filter-to-be-charged-combo #:accessor filter-to-be-charged-combo #:init-keyword #:filter-to-be-charged-combo #:init-value #f)

  (active-filter #:accessor active-filter #:init-keyword #:active-filter #:init-value #f)
  (id-set #:accessor id-set #:init-keyword #:id-set #:init-value #f)

  (sw #:accessor sw #:init-keyword #:sw #:init-value #f)
  (tv #:accessor tv #:init-keyword #:tv #:init-value #f)
  (tv-model #:accessor tv-model #:init-keyword #:tv-model #:init-value #f)
  (tv-sel #:accessor tv-sel #:init-keyword #:tv-sel #:init-value #f)
  (g-reselect-path? #:accessor g-reselect-path? #:init-value #f)

  (status-bar-1 #:accessor status-bar-1 #:init-keyword #:status-bar-1 #:init-value #f)
  (status-bar-2 #:accessor status-bar-2 #:init-keyword #:status-bar-2 #:init-value #f)
  (status-bar-3 #:accessor status-bar-3 #:init-keyword #:status-bar-3 #:init-value #f)
  (status-bar-4 #:accessor status-bar-4 #:init-keyword #:status-bar-4 #:init-value #f))

(define-method (show-me (widget <foliot/tl-widget>))
  (format #t "Widget: ~S~%" widget)
  (values))

(define (ftlw/add-id id tl-widget)
  ;; filter additional 'or' id set [ids are integers]
  ;; id might already be in the set
  (let ((ids (id-set tl-widget)))
    (cond ((not ids)
	   (set! (id-set tl-widget) (list id)))
	  ((not (memv id ids))
	   (set! (id-set tl-widget) (sort! (cons id ids) <))))
    (id-set tl-widget)))

(define (ftlw/del-id id tl-widget)
  ;; this could be called with an id that is not in the set
  (let ((ids (id-set tl-widget)))
    (and ids
	 (memv id ids)
	 (let ((new-ids (delete! id ids)))
	   (if (not (null? new-ids))
	       (begin
		 (set! (id-set tl-widget) new-ids)
		 new-ids)
	       (begin
		 (set! (id-set tl-widget) #f)
		 #f))))))


;;;
;;; Treeview related stuff
;;;

(define (ftlw/a-facturer-toggle-set-callback tl-widget model path update-db?)
  ;; (pk "ftlw/a-facturer-toggle-set-callback, path:" path)
  (let* ((row (string->number path)) ;; (current-row tl-widget)) ;; <- this is not correct the
	 ;; user may click the checkbox of another iter AND this
	 ;; callback seems to be called before the row-changed one!
	 (tuple (ftlw/get-tuple tl-widget row))
	 (id (db-foliot/get tuple 'id))
	 (iter (get-iter model path))
	 (iter-get (lambda (model iter) (fiter/get 'to-be-charged model iter)))
	 (iter-set (lambda (model iter value) (fiter/set 'to-be-charged model iter value)))
	 ;; (old-value (iter-get model iter))
	 (new-value (gtk2/fixed-toggled model iter iter-get iter-set))
	 (guicbpv? (gui-callback? tl-widget)))
    ;; faire pareil sur la case à cocher
    (set! (gui-callback? tl-widget) #f)
    (set-active (to-be-charged-cb tl-widget) new-value)
    (set! (gui-callback? tl-widget) guicbpv?)
    (when (active-filter tl-widget) (ftlw/add-id id tl-widget))
    ;; update db
    ;; (format #t "a-facturer - id: ~A, toggled row: ~S, path: ~S~%" id row path)
    (ftlw/set 'to_be_charged tl-widget (sqlite/boolean new-value) row)
    (ftlw/update-totals-status-bars tl-widget)
    (ftlw/update-store-check-position tl-widget 'to_be_charged new-value #f row)
    #f)) ;;

(define (ftlw/add-model treeview)
  (let* ((column-types (list <gchararray>
			     <gchararray>
			     <gchararray>
			     <gchararray>
			     <gchararray> ;; <gfloat> ;; duration
			     <gboolean>
			     <gchararray>
			     <gchararray>
			     <gchararray>
			     <gchararray>
			     <gchararray>))
	 (model (gtk-list-store-new column-types)))
    (set-model treeview model)
    (values model (get-selection treeview))))

(define (ftlw/add-columns tl-widget treeview)
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
	 ;; DATE
	 (renderer1 (make <gtk-cell-renderer-text>))
	 (column1   (make <gtk-tree-view-column>
		      #:title       (_ "Date")
		      #:sizing      'fixed
		      #:fixed-width (if apply-ratio? (inexact->exact (round (* dpi-ratio 90))) 90)
		      #:clickable   #f
		      #:resizable   #f
		      #:reorderable #f
		      #:alignment   .5))
	 ;; WHO
	 (renderer2 (make <gtk-cell-renderer-text>
		      #:foreground    "Blue"))
	 (column2   (make <gtk-tree-view-column>
		      #:title       (_ "Who")
		      #:sizing      'fixed
		      #:fixed-width (if apply-ratio? (inexact->exact (round (* dpi-ratio 65))) 65)
		      #:clickable   #f
		      #:resizable   #f
		      #:reorderable #f
		      #:alignment   .5))
	 ;; FOR WHOM
	 (renderer3 (make <gtk-cell-renderer-text>))
	 (column3   (make <gtk-tree-view-column>
		      #:title       (_ "For wh.")
		      #:sizing      'fixed
		      #:fixed-width (if apply-ratio? (inexact->exact (round (* dpi-ratio 65))) 65)
		      #:clickable   #f
		      #:resizable   #f
		      #:reorderable #f
		      #:alignment   .5))
	 ;; DURATION
	 #;(adjustment4 (make <gtk-adjustment>
			#:value 0
			#:lower 0
			#:upper 100
			#:step-increment .1
			#:page-increment 1
			#:page-size 0))
	 #;(renderer4 (make <gtk-cell-renderer-spin>
		      ;; #:family "Monospace"
		      #:adjustment adjustment4
		      #:climb-rate 0.1
	              #:digits 1))
	 (renderer4 (make <gtk-cell-renderer-text>
		      #:xalign      1))
	 (column4   (make <gtk-tree-view-column>
		      #:title       (_ "Dur.")
		      #:sizing      'fixed
		      #:fixed-width (if apply-ratio? (inexact->exact (round (* dpi-ratio 50))) 50)
		      #:clickable   #f
		      #:resizable   #f
		      #:reorderable #f
		      #:alignment   .5))
	 ;; A FACTURER
	 (renderer5 (make <gtk-cell-renderer-toggle>))
	 (column5   (make <gtk-tree-view-column>
		       #:title       (_ "C")
		       #:sizing      'fixed
		       #:fixed-width 30
		       #:alignment   .5))
	 ;; WHAT
	 (renderer6 (make <gtk-cell-renderer-text>))
	 (column6   (make <gtk-tree-view-column>
		      #:title       (_ "What")
		      #:clickable   #f
		      #:resizable   #f
		      #:reorderable #f
		      #:alignment   .5))
	 ;; ROW BACKGROUND COLOUR
	 (renderer7 (make <gtk-cell-renderer-text>
		      #:xalign      1))
	 (column7   (make <gtk-tree-view-column>
		      #:visible     #f))
	 ;; ROW FOREGROUND COLOUR
	 (renderer8 (make <gtk-cell-renderer-text>
		      #:xalign      1))
	 (column8   (make <gtk-tree-view-column>
		      #:visible     #f))
	 ;; ICOLOUR BACKGROUND
	 (renderer9 (make <gtk-cell-renderer-text>
		      #:xalign      1))
	 (column9   (make <gtk-tree-view-column>
		      #:visible     #f))
	 ;; ICOLOUR FOREGROUND
	 (renderer10 (make <gtk-cell-renderer-text>
		      #:xalign      1))
	 (column10   (make <gtk-tree-view-column>
		      #:visible     #f))
	 (to-pack   `((icolour ,column0 ,renderer0 "text")
		      (date ,column1 ,renderer1 "text")
		      (who ,column2 ,renderer2 "text")
		      (for-whom ,column3 ,renderer3 "text")
		      (duration ,column4 ,renderer4 "text")
		      (a-facturer ,column5 ,renderer5 "active")
		      (what ,column6 ,renderer6 "text")
		      (bg ,column7 ,renderer7 "text")
		      (fg ,column8 ,renderer8 "text")
		      (ibg ,column9 ,renderer9 "text")
		      (ifg ,column10 ,renderer10 "text"))))
    (gtk2/pack-tv-cols treeview to-pack)
    #;(set renderer4 'digits 1)
    ;; background colour
    (add-attribute column0 renderer0 "cell-background" 9)
    (add-attribute column1 renderer1 "cell-background" 7)
    (add-attribute column2 renderer2 "cell-background" 7)
    (add-attribute column3 renderer3 "cell-background" 7)
    (add-attribute column4 renderer4 "cell-background" 7)
    (add-attribute column5 renderer5 "cell-background" 7)
    (add-attribute column6 renderer6 "cell-background" 7)
    ;; foreground colour
    (add-attribute column0 renderer0 "foreground" 10)
    (add-attribute column1 renderer1 "foreground" 8)
    (add-attribute column2 renderer2 "foreground" 8)
    (add-attribute column3 renderer3 "foreground" 8)
    (add-attribute column4 renderer4 "foreground" 8)
    ;; (add-attribute column5 renderer5 "foreground" 8) ;; no foreground attribute for toggles
    (add-attribute column6 renderer6 "foreground" 8)
    (set-search-column treeview 1)
    (connect renderer5 ;; a-facturer
	     'toggled
	     (lambda (widget path)
	       (when (gui-callback? tl-widget)
		 ;; (pk "iter set callback, a-facturer")
		 (ftlw/a-facturer-toggle-set-callback tl-widget model path #t))))))

(define (ftlw/setup-treeview tl-widget)
  (let ((treeview (tv tl-widget)))
    (receive (model selection)
	(ftlw/add-model treeview)
      (set-mode selection 'single)
      (set! (tv-model tl-widget) model)
      (set! (tv-sel tl-widget) selection))
    (ftlw/add-columns tl-widget treeview)
    tl-widget))


;;;
;;; Later to be generalized
;;;

(define (ftlw/build-filter-conditions-sepxr tl-widget)
  (let* ((date-f (gtk2/get-text (filter-date-entry tl-widget) 'trim))
	 (who-f (string-escape-sql (gtk2/get-text (filter-who-entry tl-widget) 'trim)))
	 (for-whom-f (string-escape-sql (gtk2/get-text (filter-for-whom-entry tl-widget) 'trim)))
	 (what-f (string-escape-sql (gtk2/get-text (filter-what-entry tl-widget) 'trim)))
	 (description-f (string-escape-sql (gtk2/get-text (filter-description-entry tl-widget) 'trim)))
	 (to-be-charged-f (ftlw/filter-to-be-charged? tl-widget)))
    (and (or (not (string-null? date-f))
	     (not (string-null? who-f))
	     (not (string-null? for-whom-f))
	     (not (string-null? what-f))
	     (not (string-null? description-f))
	     to-be-charged-f)
	 (if to-be-charged-f
	     `(("date_" date ,date-f)
	       ("who" text ,who-f)
	       ("for_whom" text ,for-whom-f)
	       ("what" text ,what-f)
	       ("description" text ,description-f)
	       ("to_be_charged" text ,(ftlw/filter-to-be-charged-sqlvalue tl-widget)))
	     `(("date_" date ,date-f)
	       ("who" text ,who-f)
	       ("for_whom" text ,for-whom-f)
	       ("what" text ,what-f)
	       ("description" text ,description-f))))))

(define (ftlw/filter-to-be-charged? tl-widget)
  (case (get-active (filter-to-be-charged-combo tl-widget))
    ((0) #f) ;; it is not used as a filter criteria
    ((1) "to_be_charged = 't'") ;; used as a filter criteria and must be true
    ((2) "to_be_charged = 't'"))) ;; used as a filter criteria and must be false

(define (ftlw/filter-to-be-charged-sqlvalue tl-widget)
  (case (get-active (filter-to-be-charged-combo tl-widget))
    ((1) "t")
    ((2) "f")
    (else
     (format #t "ftlw/filter-to-be-charged-sqlvalue should not have been called!~%")
     #f)))

(define (ftlw/filter-to-be-charged-value tl-widget)
  (case (get-active (filter-to-be-charged-combo tl-widget))
    ((1) #t)
    ((2) #f)
    (else
     (format #t "ftlw/filter-to-be-charged-value should not have been called!~%")
     #f)))

(define (ftlw/update-status-bar-1 tl-widget)
  (let ((status-bar (status-bar-1 tl-widget))
	(new-row (current-row tl-widget))
	(tuples-nb (and (db-tuples tl-widget)
			(length (db-tuples tl-widget)))))
    (gtk2/status-pop status-bar "")
    #;(set-markup (db-name-lb2 tl-widget)
		(format #f "<span foreground=\"#777777\"><b>[ ~A: ~A ]</b></span>"
			(_ "total records") 33))
    (if (and tuples-nb (> tuples-nb 0))
	(gtk2/status-push status-bar (format #f "Record: ~A/~A" (1+ new-row) tuples-nb) "")
	(gtk2/status-push status-bar "Record: none" ""))))

(define (ftlw/select-ctime tl-widget)
  ;; Foliot not only allows the user to define and activate a filter,
  ;; but to work from there [on the subset] and in particular he may
  ;; add entries and/or delete/modify them. As he does this, we keep
  ;; track of the ids of these added/modified 'records' and
  ;; dynamically change the SQL where clause to keep them in the
  ;; list-store.

  ;; Nice :) but in order to properly compute the 'to-be-charged total
  ;; time of this listed subset, we need to rewrite the where clause
  ;; which is like [as an example]:
  ;;	 where for_whom like '%lpdi%'
  ;;	  [and to_be_charged = 't']
  ;;	    or id in (100, 101, 102)
  ;; and must instead become:
  ;; 	 where (for_whom like '%lpdi%' or id in (100, 101, 102))
  ;;	   and to_be_charged = 't'

  ;; So, in order to be able to reuse the same db-foliot/select-some
  ;; function we rebuild the where clause here.

  (let* ((filter? (active-filter tl-widget))
	 (id-set? (id-set tl-widget))
	 (f-tbc? (ftlw/filter-to-be-charged? tl-widget))
	 (f-tbc-true? (and f-tbc? (ftlw/filter-to-be-charged-value tl-widget)))
	 (clause (cond ((not filter?) "to_be_charged = 't'")
		       ((not id-set?)
			(if f-tbc-true?
			    filter?
			    (if (and f-tbc? (not f-tbc-true?))
				#f
				(format #f "(~A) and to_be_charged = 't'" filter?))))
		       (id-set?
			(let* ((filter-conditions (ftlw/build-filter-conditions-sepxr tl-widget))
			       (new-filter (and filter-conditions (dbf/get-filter filter-conditions))))
			  (format #f "((~A) or id in ~A) and to_be_charged = 't'"
				  new-filter
				  (sqlite/build-set-expression id-set?))))
		       (else
			(format #t "did I missed something? ftlw/select-ctime uncoverd condition...~%")
			#f))))
    (if clause
	(begin
	  ;; (format #t "select-ctime: ~S~%" clause)
	  (db-foliot/select-some clause #f "sum(duration)"))
	;; no clause can always return (#(#f)) which is
	;; what db-foliot/select-some would return
	;; anyway
	(list (make-vector 1 #f)))))

(define (ftlw/get-hour-hours nb)
  (if (or (float<? nb 1.0) (float=? nb 1.0)) (_ "hour") (_ "hours")))

(define (ftlw/get-hr-hrs nb)
  (if (or (float<? nb 1.0) (float=? nb 1.0)) (_ "hr") (_ "hrs")))

(define (ftlw/get-day-days nb)
  (if (or (float<? nb 1.0) (float=? nb 1.0)) (_ "day") (_ "days")))

(define (ftlw/get-totals tl-widget)
  ;; tresult, returned by guile-sqlite, is a list of 1 vector
  ;; of 1 value. the value is a float or #f [when the db or the
  ;; subset upon which the query is performed is empty].
  (let* ((tresult (db-foliot/select-some (active-filter tl-widget) (id-set tl-widget) "sum(duration)"))
	 (tduration (vector-ref (car tresult) 0))
	 (ttime (and tduration (float-round tduration 1)))
	 (tdays (and ttime (float-round (/ ttime 8) 1)))
	 (cresult (ftlw/select-ctime tl-widget))
	 (cduration (vector-ref (car cresult) 0))
	 (ctime (and cduration (float-round cduration 1)))
	 (cdays (and ctime (float-round (/ ctime 8) 1))))
    (values (if ttime ttime 0.0) (if tdays tdays 0.0)
	    (if ctime ctime 0.0) (if cdays cdays 0.0))))

(define (ftlw/update-totals-status-bars tl-widget)
  (let ((ttime-sbar (status-bar-3 tl-widget)) ;; totals ...
	(ctime-sbar (status-bar-4 tl-widget))) ;; charged ...
    (gtk2/status-pop ttime-sbar "")
    (gtk2/status-pop ctime-sbar "")
    (if (null? (db-tuples tl-widget))
	(begin
	  (gtk2/status-push ttime-sbar (format #f "~A: n/a" (_ "Total")) "")
	  (gtk2/status-push ctime-sbar (format #f "~A: n/a" (_ "Charged")) ""))
	(receive (ttime tdays ctime cdays)
	    (ftlw/get-totals tl-widget)
	  ;; here we will use ngettext
	  (gtk2/status-push ttime-sbar (format #f "~A: ~A ~A - ~A ~A"
					       (_ "Total")
					       ttime (ftlw/get-hr-hrs ttime)
					       tdays (ftlw/get-day-days tdays)) "")
	  (if ctime
	      (gtk2/status-push ctime-sbar (format #f "~A: ~A ~A - ~A ~A"
						   (_ "Charged")
						   ctime (ftlw/get-hr-hrs ctime)
						   cdays (ftlw/get-day-days cdays)) "")
	      (gtk2/status-push ctime-sbar "Charged: n/a" ""))))))

(define (ftlw/set-cur-globals tl-widget row iter)
  (set! (current-row tl-widget) row)
  (set! (current-iter tl-widget) iter))

(define (ftlw/select-row tl-widget new-row . noscroll?)
  (when (gui-callback? tl-widget)
    (let ((path (list new-row))
	  (tvsel (tv-sel tl-widget)))
      (set! (gui-callback? tl-widget) #f)
      ;; (format #t "Before unselect all~%")
      (unselect-all tvsel)
      ;; (format #t "Before select-path, new-row: ~S~%" new-row)
      (select-path tvsel path)
      (ftlw/update-status-bar-1 tl-widget)
      (when (null? noscroll?)
	(scroll-to-cell (tv tl-widget) path #f #t 0.3))
      (set! (gui-callback? tl-widget) #t)))) ;; prev value was #t

(define* (ftlw/update-store-check-position tl-widget what new-value #:optional (set-iter? #t) row)
  ;; ATTENTION: il faut chercher l'iter courrant et non prendre
  ;;            la selection parce qu'il se peut que le présent appel
  ;;            résulte d'un focus-out-event provoquer par
  ;;            enregistrer-saisie-courante, ce qui signifie alors
  ;;            que la sélection a déjà changé, alors que
  ;;            current-row est toujours bon,
  ;;            c'est à dire qu'elle pointe le row qui
  ;;            était sélectionné 'juste avant'
  (let* ((model (tv-model tl-widget))
	 (old-pos (if row row (current-row tl-widget)))
	 (old-iter (get-iter model (list old-pos)))
	 (db-tuple (and old-pos (ftlw/get-tuple tl-widget old-pos)))
	 (reference (db-foliot/get db-tuple 'id))
	 (old-entry (and db-tuple (ftlw/get what tl-widget old-pos)))
	 (tuples (db-foliot/select-some (active-filter tl-widget) (id-set tl-widget)))
	 (new-pos (db-foliot/find-pos tuples 'id reference =))
	 (reordered? (not (= old-pos new-pos)))
	 (prev-gui-cb? (gui-callback? tl-widget)))
    ;; (dimfi "old / new pos / current-row: " old-pos new-pos (current-row tl-widget))
    (when set-iter? (fiter/set what model old-iter new-value))
    (when reordered?
      ;; we still need to solve things related 'filters'
      ;; if there is an active filter off course
      (let* ((new-path (list new-pos))
	     (new-iter (get-iter model new-path))
	     (new-tuple (list-ref tuples new-pos)))
	;; (dimfi "reordered " new-tuple)
	(db-foliot/set new-tuple what new-value) ;; ?!? to-be-charged maybe...
	(set! (db-tuples tl-widget) tuples)
	(set! (gui-callback? tl-widget) #t)
	(if (< new-pos old-pos)
	    (move-before model old-iter new-iter)
	    (move-after model old-iter new-iter))
	(if row
	    (begin
	      (set! (g-reselect-path? tl-widget) new-path)
	      (ftlw/set-cur-globals tl-widget new-pos new-iter))
	    (ftlw/select-row tl-widget new-pos))
	;; (dimfi "reordered: old / new pos / current-row: " old-pos new-pos (current-row tl-widget))
	(set! (gui-callback? tl-widget) prev-gui-cb?)))))

(define (ftlw/entry-std-cb entry tl-widget what list-store-col-pos empty-msg . date?)
  (let* ((old-pos (current-row tl-widget))
	 (db-tuple (and old-pos
			(>= old-pos 0)
			(ftlw/get-tuple tl-widget old-pos)))
	 (reference (and db-tuple (db-foliot/get db-tuple 'id)))
	 (old-entry (and db-tuple (ftlw/get what tl-widget old-pos)))
	 (new-entry (gtk2/get-text entry)))
    ;; (format #t "Old: ~S, New: ~S~%Date?: ~S~%" old-entry new-entry date?)
    (if (and db-tuple
	     (string<> old-entry new-entry))
	;; l'entrée ne peut être vide si empty-msg n'est pas #f
	(if (and empty-msg
		 (string-null? new-entry))
	    (begin
	      (gdk-beep)
	      (gtk2/status-pop (status-bar-2 tl-widget) "")
	      (gtk2/status-push (status-bar-2 tl-widget) empty-msg "")
	      (gtk2/set-text entry old-entry))
	    (receive (updated? reordered?)
		(if (null? date?)
		    (db-foliot/update db-tuple what new-entry)
		    (if (date/valid-date? new-entry)
			(db-foliot/update db-tuple what (date/iso-date new-entry) new-entry)
			(values #f #f)))
	      (gtk2/status-pop (status-bar-2 tl-widget) "")
	      (if updated?
		  (begin
		    (when (active-filter tl-widget)
		      (ftlw/add-id reference tl-widget))
		    (when list-store-col-pos
		      (ftlw/update-store-check-position tl-widget what new-entry)))
		  (begin
		    (gdk-beep)
		    (gtk2/status-pop (status-bar-2 tl-widget) "")
		    (gtk2/status-push (status-bar-2 tl-widget)
                                      "Invalid date. The previous value is restored" "")
		    (gtk2/set-text entry old-entry)
		    )))))
    ;; must return #f, this is imposed by gtk+ 2.x
    #f))


;;;
;;; Config
;;;

(define (ftlw/write-config tl-widget . rests)
  (receive (win-x win-y)
      (get-position (dialog tl-widget))
    (receive (win-w win-h)
	(get-size (dialog tl-widget))
      (write-config "foliot"
			(if (null? rests)
			    (list (cons 'db-file (fcfg/get 'db-file))
				  (cons 'open-at-startup (fcfg/get 'open-at-startup))
				  (cons 'ulogo (fcfg/get 'ulogo))
				  (cons 'win-x win-x)
				  (cons 'win-y win-y)
				  (cons 'win-w win-w)
				  (cons 'win-h win-h))
			    (list (cons 'db-file (car rests))
				  (cons 'open-at-startup (cadr rests))
				  (cons 'ulogo (caddr rests))
				  (cons 'win-x win-x)
				  (cons 'win-y win-y)
				  (cons 'win-w win-w)
				  (cons 'win-h win-h))))))
    (fcfg/get 'reload))


;;;
;;; Open DB
;;;

(define (ftlw/create-db-checks db-file)
  ;; we need write access on the basename
  (let ((directory (dirname db-file)))
    (cond ((access? db-file F_OK) 'exists)
	  ((not (access? directory W_OK)) 'wrong-perm)
	  (else 'ok))))

(define (ftlw/open-db-checks db-file)
  ;; this will be called by fc/connect 'open mode or ftlw/open-db
  ;; based on foliot.config infos. in both of these cases, it must
  ;; check that:
  ;;   a. the file [still] exists;
  ;;   b. that it is W_OK;
  ;;   c. that it is an sqlite db;
  ;;   d. which does have the foliot schema
  (let ((db #f))
    (values (cond ((not (access? db-file F_OK)) 'does-not-exist)
		  ((not (access? db-file W_OK)) 'wrong-perm)
		  ((not (sqlite/sqlite-db-file? db-file)) 'not-an-sqlite-file)
		  (else
		   (set! db (db-con/open db-file #f))
		   (case (db/check-schema db)
		     ((complete) 'opened)
		     ((partial) 'opened-partial-schema)
		     ((none) 'opened-no-schema))))
	    db)))

(define (ftlw/post-open-db-ops tl-widget db-fname open-at-startup? ulogo db)
  ;; the list-store related operations that needs to be done @ connection time is
  ;; exactly what needs to be done when clearing a filter _but_ (1) filling the
  ;; combos _and_ (2) selecting the first row [this is because filter-clear will
  ;; try to (re)select the row that was active before it's been called.
  (db-con/set-db-con db (basename db-fname))
  (set! (active-filter tl-widget) #t)
  (set! (db-file tl-widget) db-fname)
  (ftlw/write-config tl-widget db-fname open-at-startup? ulogo)
  (set-markup (db-name-lb1 tl-widget)
	      (format #f "<span foreground=\"#777777\"><b>[ ~A ]</b></span>"
		      (basename db-fname)))
  (run-shinning-room-237-checks db)
  (ftlw/filter-clear tl-widget 'fillcombos)
  (sqlite/table-names db #:refresh #t)
  (unless (= (current-row tl-widget) 0)
    (ftlw/select-row tl-widget 0)))

(define (ftlw/open-db tl-widget db-filename from-gui? mode open-at-startup? checks-result db)
  ;; when basic checks passed, the schema is tested and for this the db is opened
  ;; already: -> db [the argument] is either #f or a db connector
  (case mode
    ((open)
     (case checks-result
       ((opened) (ftlw/post-open-db-ops tl-widget db-filename open-at-startup? (fcfg/get 'ulogo) db))
       ((opened-partial-schema)
	(md2b/select-gui (dialog tl-widget)
			 (_ "Confirm dialog")
			 (_ "Complete schema")
			 (_ (format #f "This database [~A] has an incomplete GNU Foliot schema, would you like to complete it now?"
				    (basename db-filename)))
			 (lambda ()
			   (db/complete-schema db)
			   (ftlw/post-open-db-ops tl-widget db-filename open-at-startup? (fcfg/get 'ulogo) db))
			 (lambda ()
			   ;; Notes: [a] an open at start-up incomplete schema db cancel operation must set
			   ;; GNU Foliot to its no db mode; [b] a connect to an incomplete schema db cancel is
			   ;; ok, [dialogs closed, prev connected db in use].
			   (if (db-con)
			       'nothing
			       (ftlw/no-db-mode tl-widget))))
	#f) ;;
       ((opened-no-schema)
	(md2b/select-gui (dialog tl-widget)
			 (_ "Confirm dialog")
			 (_ "Add schema")
			 (_ "This database does not contain the GNU Foliot schema, would you like to add it now?")
			 (lambda ()
			   (db/add-schema db)
			   (ftlw/post-open-db-ops tl-widget db-filename open-at-startup? (fcfg/get 'ulogo) db))
			 (lambda ()
			   ;; Notes: [a] an open at start-up no-schema db is not possible; [b] a connect to
			   ;; a no schema db cancel is ok [dialogs closed, prev connected db in use].
			   (if (db-con)
			       'nothing
			       (ftlw/no-db-mode tl-widget))))

	#f)))
    ((create)
     ;; (format #t "ftlw/open-db: ~S ~S~%" mode db-filename)
     (let ((db (db-con/open db-filename #f)))
       (db/add-schema db)
       (ftlw/post-open-db-ops tl-widget db-filename open-at-startup? (fcfg/get 'ulogo) db)))))


;;;
;;; Combos related
;;;

(define (ftlw/fill-combo tl-widget values-acc combo-acc db-field)
  (let ((values (db-foliot/select-distinct db-field 'add-empty)))
    (set! (values-acc tl-widget) values)
    (gtk2/fill-combo (combo-acc tl-widget) values)))

(define (ftlw/fill-combos tl-widget)
  (for-each (lambda (vcd)
	       (ftlw/fill-combo tl-widget (car vcd) (cadr vcd) (caddr vcd)))
      `((,whos ,who-combo who)
	(,for-whoms ,for-whom-combo for_whom)
	(,whats ,what-combo what))))

(define (ftlw/trace-combo-callback tl-widget combo entry db-fname in-store? signal)
  (when (ref %foliot-store 'debug)
    (let* ((row (current-row tl-widget))
	   (db-tuple (ftlw/get-tuple tl-widget row))
	   (id (db-foliot/get db-tuple 'id)))
      (dimfi (format #f "id ~A" id) db-fname signal (get-active combo) (get-text entry) in-store?))))

(define (ftlw/connect-combos-1 combos-defs)
  (for-each (lambda (combo-def)
	      (let* ((tl-widget (list-ref combo-def 0))
		     (which-combo (list-ref combo-def 1))
		     (combo (which-combo tl-widget))
		     (which-entry (list-ref combo-def 2))
		     (entry (which-entry tl-widget))
		     (db-fname (list-ref combo-def 3))
		     (in-store? (list-ref combo-def 4))
		     (kw-acc (list-ref combo-def 5))
		     (db-get-values-func (list-ref combo-def 6)))
		(connect combo
			 'changed
			 (lambda (combo)
			   (when (gui-callback? tl-widget)
			     (let* ((row (current-row tl-widget))
				    (tuple (ftlw/get-tuple tl-widget row))
				    (id (db-foliot/get tuple 'id))
				    (value (get-text entry)))
			       (ftlw/trace-combo-callback tl-widget combo entry db-fname in-store? 'changed)
			       (ftlw/set db-fname tl-widget value row)
			       ;; if active-filter, we add the id to the set, even if we are not
			       ;; certain this is absolutely necessary, since the cost of checking would
			       ;; actually be much much higher.
			       (when (active-filter tl-widget) (ftlw/add-id id tl-widget))
			       (when in-store? (ftlw/update-store-check-position tl-widget db-fname value))))))
		(connect combo
			 'move-active
			 (lambda (combo scroll-type)
			   (ftlw/trace-combo-callback tl-widget combo entry db-fname in-store? 'move-active)))
		(connect combo
			 'popup
			 (lambda (combo)
			   (ftlw/trace-combo-callback tl-widget combo entry db-fname in-store? 'popup)))
		(connect entry
			 'focus-in-event
			 (lambda (entry event)
			   (ftlw/trace-combo-callback tl-widget combo entry db-fname in-store? 'focus-in)
			   #f)) ;; do not stop - proceed with internal methods
		(connect entry
			 'focus-out-event
			 (lambda (entry event)
			   (let ((active (get-active combo))
				 (value (get-text entry))
				 (new-values (db-get-values-func)))
			     (ftlw/trace-combo-callback tl-widget combo entry db-fname in-store? 'focus-out)
			     (set! (gui-callback? tl-widget) #f)
			     (set! (kw-acc tl-widget) new-values)
			     (gtk2/fill-combo combo new-values)
			     (set-active combo (gtk2/combo-find-row combo value))
			     (set! (gui-callback? tl-widget) #t))
			   #f))))  ;; do not stop - proceed with internal methods
      combos-defs))

(define (ftlw/connect-combos tl-widget)
  (ftlw/connect-combos-1 `((,tl-widget ,who-combo ,who-entry who #t ,whos
				       ,(lambda () (db-foliot/select-distinct 'who #t)))
			   (,tl-widget ,for-whom-combo ,for-whom-entry for_whom #t ,for-whoms
				       ,(lambda () (db-foliot/select-distinct 'for_whom #t)))
			   (,tl-widget ,what-combo ,what-entry what #t ,whats
				       ,(lambda () (db-foliot/select-distinct 'what #t))))))


;;;
;;; API
;;;

(define (ftlw/get-tuple tl-widget row)
  (let ((tuples (db-tuples tl-widget)))
    (if (and (not (null? tuples))
	     (>= row 0))
	(list-ref tuples row)
	#f)))

(define (ftlw/get what tl-widget . row)
  (let* ((which-row (if (null? row) (current-row tl-widget) (car row)))
	 (db-tuple (ftlw/get-tuple tl-widget which-row)))
    ;; (format #t "ftlw/get: row: ~S, what: ~S, tuple: ~S~%" which-row what db-tuple)
    (db-foliot/get db-tuple what)))

(define (ftlw/set what tl-widget value . row)
  (let* ((which-row (if (null? row) (current-row tl-widget) (car row)))
	 (db-tuple (ftlw/get-tuple tl-widget which-row)))
    ;; (format #t "~S~%" db-tuple)
    (db-foliot/update db-tuple what value)))

(define (ftlw/fill-tv tl-widget)
  ;; note that icolors alist is like this [example]:
  ;; '((1 "#ac5251" . "#000000") (0 "#60a8a8" . "#000000"))
  (let ((model (tv-model tl-widget))
	(icolours (db-idb/get-colour-alist)))
    (gtk-list-store-clear model)
    (for-each (lambda (tuple)
		(let ((idb (db-foliot/get tuple 'imported_db)))
		  (if (= idb -1)
		      (fiter/append-fill model
					 (db-foliot/get tuple 'date_)
					 (db-foliot/get tuple 'who)
					 (db-foliot/get tuple 'for_whom)
					 (db-foliot/get tuple 'duration)
					 (sqlite/true? (db-foliot/get tuple 'to_be_charged))
					 (db-foliot/get tuple 'what)
					 #f
					 #f)
		      (fiter/append-fill model
					 (db-foliot/get tuple 'date_)
					 (db-foliot/get tuple 'who)
					 (db-foliot/get tuple 'for_whom)
					 (db-foliot/get tuple 'duration)
					 (sqlite/true? (db-foliot/get tuple 'to_be_charged))
					 (db-foliot/get tuple 'what)
					 ;; "#60a8a8" ;; sobe
					 ;; "#784800" ;; dirt
					 ;; "#000000"
					 (car (assoc-ref icolours idb))
					 (cdr (assoc-ref icolours idb))))))
	(db-tuples tl-widget))))

(define (ftlw/apply-xft-dpi-ratio tl-widget)
  (when (ref %xft-store 'apply-scale-factor?)
    (let* ((dpi-ratio (ref %xft-store 'scale-factor))
	   (widget-size-dates (inexact->exact (round (* dpi-ratio (get (date-entry tl-widget) 'width-request)))))
	   (widget-size-whos (inexact->exact (round (* dpi-ratio (get (who-combo tl-widget) 'width-request))))))
      (set (reference-entry tl-widget) 'width-request widget-size-dates)
      (set (date-entry tl-widget) 'width-request widget-size-dates)
      (set (date-edit tl-widget) 'width-request
	   (+ 20 ;; the size of the popup button
	      (inexact->exact (round (* dpi-ratio (get (date-edit tl-widget) 'width-request))))))
      (set (who-combo tl-widget) 'width-request widget-size-whos)
      (set (for-whom-combo tl-widget) 'width-request widget-size-whos)
      (set (duration-sb tl-widget) 'width-request (inexact->exact (round (* dpi-ratio (get (duration-sb tl-widget) 'width-request)))))
      (set (filter-date-entry tl-widget) 'width-request widget-size-dates)
      (set (filter-who-entry tl-widget) 'width-request widget-size-dates)
      (set (filter-for-whom-entry tl-widget) 'width-request widget-size-dates)
      ;; dpi-ratio is too much in this case
      (set (dialog tl-widget) 'width-request (inexact->exact (round (* .9 (get (dialog tl-widget) 'width-request)))))
      (set (dialog tl-widget) 'height-request (inexact->exact (round (* .9 (get (dialog tl-widget) 'height-request)))))
      (set (sw tl-widget) 'height-request (inexact->exact (round (* .9 (get (sw tl-widget) 'height-request)))))
      #;(format #t "
      reference: ~S
      date: ~S
      who: ~S
      filter date: ~S~%"
	      (get (reference-entry tl-widget) 'width-request)
	      (get (date-entry tl-widget) 'width-request)
	      (get (who-combo tl-widget) 'width-request)
	      (get (filter-date-entry tl-widget) 'width-request)
	      ))))

(define (ftlw/make-tl-widget uname gfile)
  (let* ((xmlc (glade-xml-new gfile  #f "foliot"))
	 (t-tip (gtk-tooltips-new))
	 (tl-widget (make <foliot/tl-widget>
		      #:user-name uname
		      #:glade-file gfile
		      #:xml-code xmlc
		      #:dialog (get-widget xmlc "foliot")
		      #:menubar (get-widget xmlc "foliot/menubar")
		      #:tooltip t-tip
		      #:sorting-lb (get-widget xmlc "foliot/sorting_lb")
		      #:sorting-combo (get-widget xmlc "foliot/sorting_combo")

		      #:con-bt (get-widget xmlc "foliot/con_bt")
		      #:import-bt (get-widget xmlc "foliot/import_bt")
		      #:quit-bt (get-widget xmlc "foliot/quit_bt")
		      #:dup-bt (get-widget xmlc "foliot/dup_bt")
		      #:add-bt (get-widget xmlc "foliot/add_bt")
		      #:del-bt (get-widget xmlc "foliot/del_bt")
		      #:print-bt (get-widget xmlc "foliot/print_bt")

		      #:first-bt (get-widget xmlc "foliot/first_bt")
		      #:prev-bt (get-widget xmlc "foliot/prev_bt")
		      #:next-bt (get-widget xmlc "foliot/next_bt")
		      #:last-bt (get-widget xmlc "foliot/last_bt")
		      ;; #:help-bt (get-widget xmlc "foliot/help_bt")
		      #:prefs-bt (get-widget xmlc "foliot/prefs_bt")
		      #:reference-lb (get-widget xmlc "foliot/reference_lb")
		      #:reference-entry (get-widget xmlc "foliot/reference_entry")
		      #:reference-eb (get-widget xmlc "foliot/reference_eb")
		      #:date-lb (get-widget xmlc "foliot/date_lb")
		      #:date-entry (get-widget xmlc "foliot/date_entry")
		      #:date-icon (get-widget xmlc "foliot/date_icon")
		      #:date-edit (get-widget xmlc "foliot/date_edit")

		      #:who-lb (get-widget xmlc "foliot/who_lb")
		      #:who-combo (get-widget xmlc "foliot/who_combo")
		      #:who-entry (gtk-bin-get-child (get-widget xmlc "foliot/who_combo"))

		      #:for-whom-lb (get-widget xmlc "foliot/for_whom_lb")
		      #:for-whom-combo (get-widget xmlc "foliot/for_whom_combo")
		      #:for-whom-entry (gtk-bin-get-child (get-widget xmlc "foliot/for_whom_combo"))

		      #:duration-lb (get-widget xmlc "foliot/duration_lb")
		      #:duration-sb (get-widget xmlc "foliot/duration_sb")

		      #:what-lb (get-widget xmlc "foliot/what_lb")
		      #:what-combo (get-widget xmlc "foliot/what_combo")
		      #:what-entry (gtk-bin-get-child (get-widget xmlc "foliot/what_combo"))
		      ;;#:what-tv (get-widget xmlc "foliot/what_tv")

		      #:to-be-charged-cb (get-widget xmlc "foliot/to_be_charged_cb")
		      #:description-lb (get-widget xmlc "foliot/description_lb")
		      #:description-sw (get-widget xmlc "foliot/description_sw")
		      #:description-entry (get-widget xmlc "foliot/description_entry")

		      #:db-name-lb1 (get-widget xmlc "foliot/db_name_lb1")
		      #:db-name-lb2 (get-widget xmlc "foliot/db_name_lb2")
		      #:db-name-lb3 (get-widget xmlc "foliot/db_name_lb3")

		      #:filter-icon (get-widget xmlc "foliot/filter_icon")
		      #:filter-criteria-lb (get-widget xmlc "foliot/filter_criteria_lb")
		      #:filter-apply-bt (get-widget xmlc "foliot/filter_apply_bt")
		      #:filter-clear-bt (get-widget xmlc "foliot/filter_clear_bt")
		      #:filter-select-bt (get-widget xmlc "foliot/filter_select_bt")

		      #:filter-date-lb (get-widget xmlc "foliot/filter_date_lb")
		      #:filter-date-entry (get-widget xmlc "foliot/filter_date")
		      #:filter-who-lb (get-widget xmlc "foliot/filter_who_lb")
		      #:filter-who-entry (get-widget xmlc "foliot/filter_who")
		      #:filter-for-whom-lb (get-widget xmlc "foliot/filter_for_whom_lb")
		      #:filter-for-whom-entry (get-widget xmlc "foliot/filter_for_whom")
		      #:filter-what-lb (get-widget xmlc "foliot/filter_what_lb")
		      #:filter-what-entry (get-widget xmlc "foliot/filter_what")
		      #:filter-description-lb (get-widget xmlc "foliot/filter_description_lb")
		      #:filter-description-entry (get-widget xmlc "foliot/filter_description")
		      #:filter-to-be-charged-lb (get-widget xmlc "foliot/filter_to_be_charged_lb")
		      #:filter-to-be-charged-combo (get-widget xmlc "foliot/filter_to_be_charged_combo")

		      #:sw (get-widget xmlc "foliot/sw")
		      #:tv (get-widget xmlc "foliot/tv")
		      #:status-bar-1 (get-widget xmlc "foliot/status_bar_1")
		      #:status-bar-2 (get-widget xmlc "foliot/status_bar_2")
		      #:status-bar-3 (get-widget xmlc "foliot/status_bar_3")
		      #:status-bar-4 (get-widget xmlc "foliot/status_bar_4"))))
    (ftlw/setup-treeview tl-widget)
    ;; the combos need to be cleared since some example items
    ;; are defined in the glade file
    (gtk2/clear-combo (who-combo tl-widget))
    (gtk2/clear-combo (for-whom-combo tl-widget))
    (gtk2/clear-combo (what-combo tl-widget))
    (ftlw/connect-combos tl-widget)
    (set-sensitive (filter-select-bt tl-widget) #f)
    (ftlw/apply-xft-dpi-ratio tl-widget)
    ;; not doing anything yet but soon will have to
    (ftlw/translate tl-widget)
    (set-markup (db-name-lb1 tl-widget) "<span foreground=\"#777777\"><b>[ ]</b></span>")
    (ftlw/set-filter-icon tl-widget 'off)
    tl-widget))


;;;
;;; Add, duplicate, delete
;;;

(define (ftlw/add tl-widget)
  (let* ((model (tv-model tl-widget))
	 (today (date/system-date))
	 (iso-today (date/iso-date today))
	 (uname (user-name tl-widget))
	 (filter? (active-filter tl-widget))
	 (restore-mode? (null? (db-tuples tl-widget)))
	 (new-id (db-foliot/add iso-today
			      uname	;; who
			      ""	;; for_whom
			      "" 	;; what
			      0		;; duration
			      "f"	;; to-be-charged
			      ""))	;; description
	 (ids? (if filter? (ftlw/add-id new-id tl-widget) (id-set tl-widget)))
	 (new-iter (fiter/prepend-fill model today uname "" 0 #f "" #f #f))
	 (tuples (db-foliot/select-some filter? ids?))
	 (new-pos (db-foliot/find-pos tuples 'id new-id =)))
    (set! (db-tuples tl-widget) tuples)
    (unless (sqlite/tuple-pos uname (whos tl-widget) string=? 0)
      (let ((prev-gui-cb? (gui-callback? tl-widget))
	    (w-combo (who-combo tl-widget)))
	(set! (gui-callback? tl-widget) #f)
	(ftlw/fill-combo tl-widget whos who-combo 'who)
	(set-active w-combo (gtk2/combo-find-row w-combo uname))
	(set! (gui-callback? tl-widget) prev-gui-cb?)))
    (when restore-mode? (ftlw/normal-mode tl-widget))
    (if (= new-pos 0)
	(ftlw/select-row tl-widget 0)
	(begin
	  (move-after model
		      new-iter
		      (get-iter model (list new-pos)))
	  (ftlw/select-row tl-widget new-pos)))))

(define (ftlw/duplicate tl-widget)
  (let* ((model (tv-model tl-widget))
	 (row (current-row tl-widget))
	 (iter (current-iter tl-widget))
	 (tuple (ftlw/get-tuple tl-widget row))
	 (filter? (active-filter tl-widget))
	 (new-id (db-foliot/duplicate (db-foliot/get tuple 'id) tuple))
	 (ids? (if filter? (ftlw/add-id new-id tl-widget) (id-set tl-widget)))
	 (new-iter (fiter/prepend-fill model
				       (fiter/get 'date model iter)
				       (fiter/get 'who model iter)
				       (fiter/get 'for-whom model iter)
				       (string->number (fiter/get 'duration model iter))
				       (fiter/get 'to-be-charged model iter)
				       (fiter/get 'what model iter)
				       #f
				       #f))
	 (tuples (db-foliot/select-some filter? ids?))
	 (new-pos (db-foliot/find-pos tuples 'id new-id =)))
    #;(dimfi row new-id new-pos)
    (set! (db-tuples tl-widget) tuples)
    (unselect-all (tv-sel tl-widget))
    (if (< new-pos row)
	(move-before model new-iter (get-iter model (list new-pos)))
	(move-after model new-iter (get-iter model (list new-pos))))
    (ftlw/select-row tl-widget new-pos)
    (ftlw/update-totals-status-bars tl-widget)))

(define (ftlw/delete-msg-str)
  ;; with "~10,,,' @A" it would be right justified but because this is
  ;; passed to a gtk label widget, which uses variable size font, it
  ;; is not sufficient and not as nice at it should.
  "Are you sure you want to delete this entry ?

	~10,,,' A: ~A
	~10,,,' A: ~A
	~10,,,' A: ~A
	~10,,,' A: ~A")

(define (ftlw/delete tl-widget)
  (let* ((model (tv-model tl-widget))
	 (row (current-row tl-widget))
	 (last-row (1- (gtk-tree-model-iter-n-children model #f)))
	 (iter (current-iter tl-widget))
	 (tuple (ftlw/get-tuple tl-widget row))
	 (reference (db-foliot/get tuple 'id)))
    (md2b/select-gui (dialog tl-widget)
		     (_ "Confirm dialog")
		     (_ "Deletion")
		     (format #f "~?" (ftlw/delete-msg-str)
			     (list "Reference" reference
				   "Date" (db-foliot/get tuple 'date_)
				   "Who" (db-foliot/get tuple 'who)
				   "for whom" (db-foliot/get tuple 'for_whom)))
		     (lambda ()
		       (db-foliot/delete reference)
		       (let* ((a-filter? (active-filter tl-widget))
			      (new-id-set (and a-filter? (ftlw/del-id reference tl-widget)))
			      (tuples (db-foliot/select-some a-filter? new-id-set))
			      (its-length (length tuples)))
			 (set! (db-tuples tl-widget) tuples)
			 (remove model iter)
			 (if (> its-length 0)
			     (if (= row last-row)
				 (ftlw/select-row tl-widget (1- last-row) 'noscroll)
				 (ftlw/select-row tl-widget row 'noscroll))
			     (ftlw/empty-subset-or-db-mode tl-widget))
			 (ftlw/update-totals-status-bars tl-widget)))
		     (lambda () 'nothing)
		     'dialog-warning)))


;;;
;;; Other GUI related
;;;

(define (ftlw/check-nav-tb-sensitive-needs tl-widget line-nb)
  (let ((nb-rows (length (db-tuples tl-widget))))
    (gtk2/check-nav-tb-sensitive-needs line-nb
				       nb-rows
				       (first-bt tl-widget)
				       (prev-bt tl-widget)
				       (next-bt tl-widget)
				       (last-bt tl-widget)
				       (list))))


;;;
;;; Filter related
;;;

(define (ftlw/empty-subset-or-db-mode tl-widget)
  (let ((prev-gui-cb? (gui-callback? tl-widget)))
    (set! (gui-callback? tl-widget) #f)
    (set! (current-row tl-widget) -1)
    (set! (current-iter tl-widget) -1)
    (set-markup (reference-lb tl-widget)
		(format #f "<span foreground=\"~A\"><i>~A</i></span>" "#000000" (_ "Reference")))
    (hide (reference-eb tl-widget))
    (for-each (lambda (acc)
		(gtk2/set-text (acc tl-widget) ""))
	`(,reference-entry
	  ,date-entry
	  ,description-entry))
    (for-each (lambda (acc)
		(set-active (acc tl-widget) 0))
	`(,who-combo
	  ,for-whom-combo
	  ,what-combo))
    (set-value (duration-sb tl-widget) 0)
    (set-active (to-be-charged-cb tl-widget) #f)
    (set! (gui-callback? tl-widget) prev-gui-cb?)
    (for-each (lambda (acc)
		(set-sensitive (acc tl-widget) #t))
	`(,import-bt
	  ,add-bt))
    (for-each (lambda (acc)
		;; (format #t "empty-mode: ~S~%" acc)
		(set-sensitive (acc tl-widget) #f))
	`(,print-bt
	  ,first-bt ,prev-bt ,next-bt ,last-bt ;; (ftlw/check-nav-tb-sensitive-needs tl-widget -1)
	  ,dup-bt ,del-bt
	  ,date-entry
	  ,description-entry
	  ,who-combo
	  ,for-whom-combo
	  ,what-combo
	  ,duration-sb
	  ,to-be-charged-cb
	  ,tv))
    (unless (active-filter tl-widget)
      (for-each (lambda (acc)
		  ;; (format #t "empty-mode: ~S~%" acc)
		  (set-sensitive (acc tl-widget) #f))
	  `(,filter-apply-bt
	    ,filter-clear-bt
	    ,filter-select-bt
	    ,filter-date-entry
	    ,filter-who-entry
	    ,filter-for-whom-entry
	    ,filter-what-entry
	    ,filter-description-entry
	    ,filter-to-be-charged-combo)))
    (ftlw/update-status-bar-1 tl-widget)))

(define (ftlw/no-db-mode tl-widget)
  (ftlw/empty-subset-or-db-mode tl-widget)
  (for-each (lambda (acc)
	      (set-sensitive (acc tl-widget) #f))
      `(,import-bt
	,add-bt
	,filter-apply-bt
	,filter-clear-bt
	,filter-select-bt
	,filter-date-entry
	,filter-who-entry
	,filter-for-whom-entry
	,filter-what-entry
	,filter-description-entry
	,filter-to-be-charged-combo)))

(define (ftlw/normal-mode tl-widget)
  (for-each (lambda (acc)
	      (set-sensitive (acc tl-widget) #t))
      `(,import-bt
	,print-bt
	;; ,first-bt ,prev-bt ,next-bt ,last-bt: checked by select-row callback
	,dup-bt ,del-bt
	,date-entry
	,description-entry
	,who-combo
	,for-whom-combo
	,what-combo
	,duration-sb
	,to-be-charged-cb
	,tv
	;; if from no-db-mode ...
	,add-bt
	,filter-apply-bt
	,filter-clear-bt
	;; ,filter-select-bt
	,filter-date-entry
	,filter-who-entry
	,filter-for-whom-entry
	,filter-what-entry
	,filter-description-entry
	,filter-to-be-charged-combo)))

(define (ftlw/filter-get-row-new-pos-if-any tl-widget new-tuple-set)
  (let* ((row (current-row tl-widget))
	 (tuple (and row (>= row 0) (ftlw/get-tuple tl-widget row)))
	 (reference (and tuple (db-foliot/get tuple 'id))))
    (and reference
	 (db-foliot/find-pos new-tuple-set 'id reference =))))

(define (ftlw/-display-filter-apply-infos filter tuple-set-length new-pos)
  (format #t "Filter is: ~S~%" filter)
  (format #t "  nb of filtered tuples: ~S~%" tuple-set-length)
  (format #t "  previous active row new pos [or 0 if not in the set]: ~S~%" new-pos)
  #t)

(define (ftlw/set-filter-icon tl-widget mode)
  (let ((t-tip (tooltip tl-widget))
	(image (filter-icon tl-widget)))
    (case mode
      ((on)
       (set-from-file image (string-append (ref %foliot-store 'iconsdir) "/pie-partial-colour.svg"))
       (set-tip t-tip image (_ "ON: you are working on a subset of your database.")))
      ((off)
       (set-from-file image (string-append (ref %foliot-store 'iconsdir) "/pie-full-grey.svg"))
       (set-tip t-tip image (_ "OFF: you are working on the entire database."))))))

(define (ftlw/filter-apply tl-widget . force?)
  ;; (format #t "applying filter: ~S~%" force?)
  (let* ((prev-gui-cb? (gui-callback? tl-widget))
	 (prev-filter (active-filter tl-widget))
	 (filter-conditions? (ftlw/build-filter-conditions-sepxr tl-widget))
	 (filter? (and filter-conditions? (dbf/get-filter filter-conditions?))))
    ;; being invalid, the date only may lead to filter? being #f
    (set! (gui-callback? tl-widget) #f)
    (if filter?
	(if (or (not prev-filter)
		(not (null? force?))
		(not (string=? filter? prev-filter)))
	    ;; in this case we reset the id-set to #f
	    (begin
	      (ftlw/set-filter-icon tl-widget 'on)
	      (set! (active-filter tl-widget) filter?)
	      (set! (id-set tl-widget) #f)
	      (let* ((new-tuple-set (db-foliot/select-some filter? #f))
		     (new-pos (ftlw/filter-get-row-new-pos-if-any tl-widget new-tuple-set)))
		(when (ref %foliot-store 'debug)
                  (ftlw/-display-filter-apply-infos filter?
                                                    (length new-tuple-set)
                                                    new-pos))
		(set! (db-tuples tl-widget) new-tuple-set)
		(ftlw/fill-tv tl-widget)
		(ftlw/update-totals-status-bars tl-widget)
		(if (null? new-tuple-set)
		    (ftlw/empty-subset-or-db-mode tl-widget)
		    (begin
		      (ftlw/normal-mode tl-widget)
		      (set! (gui-callback? tl-widget) #t)
		      (if new-pos
			  (ftlw/select-row tl-widget new-pos)
			  (ftlw/select-row tl-widget 0)))))))
	(begin
	  (ftlw/set-filter-icon tl-widget 'off)
	  (when prev-filter (ftlw/filter-clear tl-widget))
	  (format #t "filter is empty~%")))
    (set! (gui-callback? tl-widget) prev-gui-cb?)))

(define (ftlw/filter-clear tl-widget . fillcombos?)
  ;; (format #t "clearing filter: ~S~%" fillcombos?)
  (let ((prev-gui-cb? (gui-callback? tl-widget)))
    (ftlw/set-filter-icon tl-widget 'off)
    (set! (gui-callback? tl-widget) #f)
    (gtk2/set-text (filter-date-entry tl-widget) "")
    (gtk2/set-text (filter-who-entry tl-widget) "")
    (gtk2/set-text (filter-for-whom-entry tl-widget) "")
    (gtk2/set-text (filter-what-entry tl-widget) "")
    (gtk2/set-text (filter-description-entry tl-widget) "")
    (set-active (filter-to-be-charged-combo tl-widget) 0)
    (when (active-filter tl-widget)
      (set! (active-filter tl-widget) #f)
      (set! (id-set tl-widget) #f)
      (let* ((new-tuple-set (db-foliot/select-all))
	     (new-pos (ftlw/filter-get-row-new-pos-if-any tl-widget new-tuple-set)))
	(set! (db-tuples tl-widget) new-tuple-set)
	(if (not (null? fillcombos?)) (ftlw/fill-combos tl-widget))
	(ftlw/fill-tv tl-widget)
	(if (null? new-tuple-set)
	    (ftlw/empty-subset-or-db-mode tl-widget)
	    (begin
	      (ftlw/normal-mode tl-widget)
	      (set! (gui-callback? tl-widget) #t)
	      (if new-pos
		  (ftlw/select-row tl-widget new-pos)
		  (ftlw/select-row tl-widget 0))))))
    (set! (gui-callback? tl-widget) #t)
    ;; this has to be last
    (ftlw/update-totals-status-bars tl-widget)
    (set! (gui-callback? tl-widget) prev-gui-cb?)))


;;;
;;; Tooltips and dialog translation
;;;

(define (ftlw/dates-tooltip-str)
  (_ "In its current version, GNU Foliot only supports the following date format: dd-mm-yyyy [*]

The date must be a valid date between the 01.01.1970 and 31.12.2037.

[*]	'.' or '/' may also be used instead of '-', but not mixed:
	the same field seperator must be used twice."
))

(define (ftlw/what-tooltip-str)
  (_ "We suggest the use of keywords seperated by '/' [1]: '/admin', '/admin/expenses', '/sysadmin/install', '/contrib/gnu/gettext', '/personal', ...

[1]	This way, the distinct database content of this field represents your activity tree. We actually plan to display it as such in the future."))

(define (ftlw/filter-date-tooltip-str)
  (_ "Date filters:
	a date
	<, <=, =, >= or > a date
	a date..another date [*]

[*] the range will include the specified dates"))

(define (ftlw/filter-text-tooltip-str)
  (_ "Text filters:
  =			is empty
  *			is not empty
  a_string		contains a_string
  \"a_string\"	contains 'a word delimited' a_string
  =a_string	strictly contains a_string
  *a_string	ends with a_string
  a_string*	starts with a_string

  & | ! 	and or not, for example:
			demu & demi | dema ! demand"))

(define (ftlw/filter-criteria-tooltip-str)
  (_ "When more then one filter field is used, they are combined using the AND operator."))


(define (ftlw/translate tl-widget)
  (let ((tb-tip (gtk-tooltips-new))
	(t-tip (tooltip tl-widget)))
    (gtk-tooltips-enable tb-tip)
    #;(set-icon-from-stock (date-entry tl-widget)
			 GTK_ENTRY_ICON_SECONDARY
			 GTK_STOCK_ABOUT)
    #;(set-markup (date-lb tl-widget) "<a href=\"www.fsf.org\">Date:</a>")
    (set-tooltip (import-bt tl-widget) tb-tip (_ "Import/Unimport...") "")

    (set-tip t-tip (date-lb tl-widget) (ftlw/dates-tooltip-str))
    ;;(set-label (what-lb tl-widget) (format #f "<u>~A:</u>" (_ "What")))
    ;;(set-tip t-tip (what-lb tl-widget) (ftlw/what-tooltip-str))
    (set-tip t-tip (filter-criteria-lb tl-widget) (ftlw/filter-criteria-tooltip-str))
    (set-label (filter-date-lb tl-widget)
	       (format #f "<u><span foreground=\"~A\">~A:</span></u>" *filters-border* (_ "Date")))
    (set-tip t-tip (filter-date-lb tl-widget) (ftlw/filter-date-tooltip-str))
    (set-label (filter-who-lb tl-widget)
	       (format #f "<u><span foreground=\"~A\">~A:</span></u>" *filters-border* (_ "Who")))
    (set-tip t-tip (filter-who-lb tl-widget) (ftlw/filter-text-tooltip-str))
    (set-label (filter-for-whom-lb tl-widget)
	       (format #f "<span foreground=\"~A\">~A:</span>" *filters-border* (_ "For whom")))
    ;; (set-tip t-tip (filter-for-whom-lb tl-widget) (ftlw/filter-text-tooltip-str))
    (set-label (filter-what-lb tl-widget)
	       (format #f "<span foreground=\"~A\">~A:</span>" *filters-border* (_ "What")))
    ;; (set-tip t-tip (filter-what-lb tl-widget) (ftlw/filter-text-tooltip-str))
    (set-label (filter-to-be-charged-lb tl-widget)
	       (format #f "<span foreground=\"~A\">~A:</span>" *filters-border* (_ "To be charged")))
    (set-label (filter-description-lb tl-widget)
	       (format #f "<span foreground=\"~A\">~A:</span>" *filters-border* (_ "Description")))
    #;(set-tip t-tip (filter-description-lb tl-widget) (ftlw/filter-text-tooltip-str))))


#!

;;;
;;; Completion code example
;;;

(who-cpl (make <gtk-entry-completion>))

;; completion assignments
(set-text-column who-cpl 0)
(set-minimum-key-length who-cpl 0)
(set-completion (who-entry tl-widget) who-cpl)
(set-text-column for-whom-cpl 0)
(set-completion (for-whom-entry tl-widget) for-whom-cpl)

!#

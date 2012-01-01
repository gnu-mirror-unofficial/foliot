;; -*- mode: scheme; coding: utf-8 -*-

;;;; Copyright (C) 2011, 2012
;;;; Free Software Foundation, Inc.
;;;;
;;;; This library is free software; you can redistribute it and/or
;;;; modify it under the terms of the GNU Lesser General Public
;;;; License as published by the Free Software Foundation; either
;;;; version 3 of the License, or (at your option) any later version.
;;;; 
;;;; This library is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;;; Lesser General Public License for more details.
;;;; 
;;;; You should have received a copy of the GNU Lesser General Public
;;;; License along with this library; if not, write to the Free Software
;;;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
;;;;

;;; Commentary:

;;; Code:

(define-module (kise tl-widget)
  ;; guile/guile-gnome
  :use-module (ice-9 format)
  :use-module (ice-9 receive)
  :use-module (oop goops)
  :use-module (gnome gnome) ;; could use the system help
  :use-module (gnome gobject)
  :use-module (gnome gtk)
  :use-module (gnome glade)

  ;; common
  :use-module (macros reexport)
  :use-module (macros push)
  :use-module (macros when)
  :use-module (macros do)
  :use-module (system dates)
  :use-module (system i18n)
  :use-module (system aglobs)
  :use-module (strings strings)
  :use-module (nbs all)
  :use-module (gtk all)
  :use-module (db sqlite)

  ;; kise
  :use-module (kise config)
  :use-module (kise colours)

  :use-module (kise db)
  :use-module (kise iter)
  ;; :use-module (kise what-tree)

  :export (<kise/tl-widget>
	   gui-callback?
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
	   dup-bt
	   add-bt
	   del-bt
	   print-bt

	   first-bt
	   prev-bt
	   next-bt
	   last-bt
	   ;; help-bt

	   reference-lb
	   reference-entry

	   date-lb
	   date-entry
	   date-icon

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
	   ;;what-tv
	   ;;what-tv-model
	   ;;what-tv-sel

	   to-be-charged-cb

	   description-lb
	   description-entry

	   db-name-lb1
	   db-name-lb2
	   db-name-entry

	   filter-apply-bt
	   filter-clear-bt
	   filter-select-bt
	   
	   filter-date-entry
	   filter-who-entry
	   filter-for-whom-entry
	   filter-what-entry
	   filter-description-entry
	   filter-to-be-charged-cb
	   filter-to-be-charged-eb
	   filter-to-be-charged-combo

	   active-filter
	   id-set

	   tv
	   tv-model
	   tv-sel
	   status-bar-1 ;; records info
	   status-bar-2 ;; app messages
	   status-bar-3 ;; total time
	   status-bar-4 ;; charged time

	   ktlw/create-db-checks
	   ktlw/open-db-checks
	   ktlw/open-db
	   ktlw/fill-combos
	   ktlw/connect-combos

	   ktlw/get-tuple
	   ktlw/get
	   ktlw/set
	   ktlw/setup-treeview
	   ktlw/fill-tv
	   ktlw/get-hour-hours
	   ktlw/get-day-days
	   ktlw/get-totals
	   ktlw/update-status-bar-1
	   ktlw/select-ctime
	   ktlw/update-totals-status-bars

	   ktlw/select-row
	   ktlw/update-store-check-position
	   ktlw/entry-std-cb
	   ktlw/make-tl-widget

	   ktlw/add-id ;; filter additional 'or' id set
	   ktlw/del-id
	   ktlw/add
	   ktlw/duplicate
	   ktlw/delete

	   ktlw/check-nav-tb-sensitive-needs
	   ktlw/empty-subset-or-db-mode
	   ktlw/no-db-mode
	   ktlw/normal-mode
	   ktlw/filter-apply
	   ktlw/filter-clear))


(eval-when (compile load eval)
  (re-export-public-interface (oop goops)
			      (gnome gobject)
			      (gnome gtk)
			      (gnome glade)
			      (kise config))
  (textdomain "tl-widget")
  (bindtextdomain "tl-widget" (aglobs/get 'pofdir)))


;;;
;;; <kise/tl-widget>
;;;

(define-class <kise/tl-widget> () ;; (<kise/config>)
  (user-name :accessor user-name :init-keyword :user-name :init-value #f)
  (gui-callback? :accessor gui-callback? :init-keyword :gui-callback? :init-value #t)

  (db-file :accessor db-file :init-keyword :db-file :init-value #f)
  (db-tuples :accessor db-tuples :init-keyword :db-tuples :init-value #f)
  (whos :accessor whos :init-keyword :whos :init-value #f)
  (for-whoms :accessor for-whoms :init-keyword :for-whoms :init-value #f)
  (whats :accessor whats :init-keyword :whats :init-value #f)
  (current-row :accessor current-row :init-keyword :current-row :init-value #f)
  (current-iter :accessor current-iter :init-keyword :current-iter :init-value #f)
  (glade-file :accessor glade-file :init-keyword :glade-file :init-value #f)
  (xml-code :accessor xml-code :init-keyword :xml-code :init-value #f)

  (dialog :accessor dialog :init-keyword :dialog :init-value #f)
  (menubar :accessor menubar :init-keyword :menubar :init-value #f)
  (tooltip :accessor tooltip :init-keyword :tooltip :init-value #f)

  (sorting-lb :accessor sorting-lb :init-keyword :sorting-lb :init-value #f)
  (sorting-combo :accessor sorting-combo :init-keyword :sorting-combo :init-value #f)

  (con-bt :accessor con-bt :init-keyword :con-bt :init-value #f)
  (dup-bt :accessor dup-bt :init-keyword :dup-bt :init-value #f)
  (add-bt :accessor add-bt :init-keyword :add-bt :init-value #f)
  (del-bt :accessor del-bt :init-keyword :del-bt :init-value #f)
  (print-bt :accessor print-bt :init-keyword :print-bt :init-value #f)

  (first-bt :accessor first-bt :init-keyword :first-bt :init-value #f)
  (prev-bt :accessor prev-bt :init-keyword :prev-bt :init-value #f)
  (next-bt :accessor next-bt :init-keyword :next-bt :init-value #f)
  (last-bt :accessor last-bt :init-keyword :last-bt :init-value #f)
  (help-bt :accessor help-bt :init-keyword :help-bt :init-value #f)

  (reference-lb :accessor reference-lb :init-keyword :reference-lb :init-value #f)
  (reference-entry :accessor reference-entry :init-keyword :reference-entry :init-value #f)
  (date-lb :accessor date-lb :init-keyword :date-lb :init-value #f)
  (date-entry :accessor date-entry :init-keyword :date-entry :init-value #f)
  (date-icon :accessor date-icon :init-keyword :date-icon :init-value #f)
  (who-lb :accessor who-lb :init-keyword :who-lb :init-value #f)
  (who-entry :accessor who-entry :init-keyword :who-entry :init-value #f)
  (who-combo :accessor who-combo :init-keyword :who-combo :init-value #f)
  (for-whom-lb :accessor for-whom-lb :init-keyword :for-whom-lb :init-value #f)
  (for-whom-entry :accessor for-whom-entry :init-keyword :for-whom-entry :init-value #f)
  (for-whom-combo :accessor for-whom-combo :init-keyword :for-whom-combo :init-value #f)
  (duration-lb :accessor duration-lb :init-keyword :duration-lb :init-value #f)
  (duration-sb :accessor duration-sb :init-keyword :duration-sb :init-value #f)
  (what-lb :accessor what-lb :init-keyword :what-lb :init-value #f)
  (what-combo :accessor what-combo :init-keyword :what-combo :init-value #f)
  (what-entry :accessor what-entry :init-keyword :what-entry :init-value #f)
  ;; (what-tv :accessor what-tv :init-keyword :what-tv :init-value #f)
  ;; (what-tv-model :accessor what-tv-model :init-keyword :what-tv-model :init-value #f)
  ;; (what-tv-sel :accessor what-tv-sel :init-keyword :what-tv-sel :init-value #f)
  (to-be-charged-cb :accessor to-be-charged-cb :init-keyword :to-be-charged-cb :init-value #f)
  (description-lb :accessor description-lb :init-keyword :description-lb :init-value #f)
  (description-entry :accessor description-entry :init-keyword :description-entry :init-value #f)

  (db-name-lb1 :accessor db-name-lb1 :init-keyword :db-name-lb1 :init-value #f)
  (db-name-lb2 :accessor db-name-lb2 :init-keyword :db-name-lb2 :init-value #f)
  (db-name-entry :accessor db-name-entry :init-keyword :db-name-entry :init-value #f)

  (filter-apply-bt :accessor filter-apply-bt :init-keyword :filter-apply-bt :init-value #f)
  (filter-clear-bt :accessor filter-clear-bt :init-keyword :filter-clear-bt :init-value #f)
  (filter-select-bt :accessor filter-select-bt :init-keyword :filter-select-bt :init-value #f)
  
  (filter-date-entry :accessor filter-date-entry :init-keyword :filter-date-entry :init-value #f)
  (filter-who-entry :accessor filter-who-entry :init-keyword :filter-who-entry :init-value #f)
  (filter-for-whom-entry :accessor filter-for-whom-entry :init-keyword :filter-for-whom-entry :init-value #f)
  (filter-what-entry :accessor filter-what-entry :init-keyword :filter-what-entry :init-value #f)
  (filter-description-entry :accessor filter-description-entry :init-keyword :filter-description-entry :init-value #f)
  (filter-to-be-charged-cb :accessor filter-to-be-charged-cb :init-keyword :filter-to-be-charged-cb :init-value #f)
  (filter-to-be-charged-eb :accessor filter-to-be-charged-eb :init-keyword :filter-to-be-charged-eb :init-value #f)
  (filter-to-be-charged-combo :accessor filter-to-be-charged-combo :init-keyword :filter-to-be-charged-combo :init-value #f)

  (active-filter :accessor active-filter :init-keyword :active-filter :init-value #f)
  (id-set :accessor id-set :init-keyword :id-set :init-value #f)

  (tv :accessor tv :init-keyword :tv :init-value #f)
  (tv-model :accessor tv-model :init-keyword :tv-model :init-value #f)
  (tv-sel :accessor tv-sel :init-keyword :tv-sel :init-value #f)
  (status-bar-1 :accessor status-bar-1 :init-keyword :status-bar-1 :init-value #f)
  (status-bar-2 :accessor status-bar-2 :init-keyword :status-bar-2 :init-value #f)
  (status-bar-3 :accessor status-bar-3 :init-keyword :status-bar-3 :init-value #f)
  (status-bar-4 :accessor status-bar-4 :init-keyword :status-bar-4 :init-value #f))

(define-method (show-me (widget <kise/tl-widget>))
  (format #t "Widget: ~S~%" widget)
  (values))

(define (ktlw/add-id id tl-widget)
  ;; filter additional 'or' id set [ids are integers]
  ;; id might already be in the set
  (let ((ids (id-set tl-widget)))
    (cond ((not ids)
	   (set! (id-set tl-widget) (list id)))
	  ((not (memv id ids))
	   (set! (id-set tl-widget) (sort! (cons id ids) <))))
    (id-set tl-widget)))

(define (ktlw/del-id id tl-widget)
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

(define (ktlw/a-facturer-toggle-set-callback tl-widget model path update-db?)
  (let* ((row (string->number path)) ;; (current-row tl-widget)) ;; <- this is not correct the
	 ;; user may click the checkbox of another iter AND this
	 ;; callback seems to be called before the row-changed one!
	 (tuple (ktlw/get-tuple tl-widget row))
	 (id (db-kise/get tuple 'id))
	 (iter (get-iter model path))
	 (iter-get (lambda (model iter) (kiseiter/get 'to-be-charged model iter)))
	 (iter-set (lambda (model iter value) (kiseiter/set 'to-be-charged model iter value)))
	 (new-value (gtk2/fixed-toggled model iter iter-get iter-set))
	 (guicbpv? (gui-callback? tl-widget)))
    ;; faire pareil sur la case à cocher
    (set! (gui-callback? tl-widget) #f)
    (set-active (to-be-charged-cb tl-widget) new-value)
    (set! (gui-callback? tl-widget) guicbpv?)
    (when (active-filter tl-widget) (ktlw/add-id id tl-widget))
    ;; update db
    ;; (format #t "a-facturer - current row: ~S, path: ~S~%" row path)
    (ktlw/set 'to_be_charged tl-widget (sqlite/boolean new-value) row)
    (ktlw/update-totals-status-bars tl-widget)))

(define (ktlw/add-model treeview)
  (let* ((column-types (list <gchararray>
			     <gchararray>
			     <gchararray>
			     <gfloat> ;; duration
			     ;; <gchararray>
			     <gboolean>
			     <gchararray>
			     ))
	 (model (gtk-list-store-new column-types)))
    (set-model treeview model)
    (values model (get-selection treeview))))

(define (ktlw/add-columns tl-widget treeview)
  (let* ((model     (get-model treeview))
	 ;; DATE
	 (renderer1 (make <gtk-cell-renderer-text>))
	 (column1   (make <gtk-tree-view-column>
		      :title       (_ "Date")
		      :sizing      'fixed
		      :fixed-width 80
		      :clickable   #f
		      :resizable   #f
		      :reorderable #f
		      :alignment   .5))
	 ;; WHO
	 (renderer2 (make <gtk-cell-renderer-text>
		      :foreground    "Blue"))
	 (column2   (make <gtk-tree-view-column>
		      :title       (_ "Who")
		      :sizing      'fixed
		      :fixed-width 65
		      :clickable   #f
		      :resizable   #f
		      :reorderable #f
		      :alignment   .5))
	 ;; FOR WHOM
	 (renderer3 (make <gtk-cell-renderer-text>))
	 (column3   (make <gtk-tree-view-column>
		      :title       (_ "For whom")
		      :sizing      'fixed
		      :fixed-width 65
		      :clickable   #f
		      :resizable   #f
		      :reorderable #f
		      :alignment   .5))
	 ;; DURATION
	 (renderer4 (make <gtk-cell-renderer-spin>
		      :family      "Monospace"
		      :climb-rate  0.1
		      :digits      1))
	 (column4   (make <gtk-tree-view-column>
		      :title       (_ "Dur.")
		      :sizing      'fixed
		      :fixed-width 37
		      :clickable   #f
		      :resizable   #f
		      :reorderable #f
		      :alignment   .5))
	 ;; A FACTURER
	 (renderer5 (make <gtk-cell-renderer-toggle>))
	 (column5   (make <gtk-tree-view-column>
		       :title       (_ "C")
		       :sizing      'fixed
		       :fixed-width 20
		       :alignment   .5))
	 ;; WHAT
	 (renderer6 (make <gtk-cell-renderer-text>))
	 (column6   (make <gtk-tree-view-column>
		      :title       (_ "What")
		      :clickable   #f
		      :resizable   #f
		      :reorderable #f
		      :alignment   .5))
	 (to-pack   `((date ,column1 ,renderer1 "text")
		      (who ,column2 ,renderer2 "text")
		      (for-whom ,column3 ,renderer3 "text")
		      (duration ,column4 ,renderer4 "text")
		      (a-facturer ,column5 ,renderer5 "active")
		      (what ,column6 ,renderer6 "text"))))
    (gtk2/pack-tv-cols treeview to-pack)
    (set renderer4 'digits 1)
    (connect renderer5 ;; a-facturer
	     'toggled
	     (lambda (widget path)
	       (ktlw/a-facturer-toggle-set-callback tl-widget model path #t)))))

(define (ktlw/setup-treeview tl-widget)
  (let ((treeview (tv tl-widget)))
    (receive (model selection)
	(ktlw/add-model treeview)
      (set-mode selection 'single)
      (set! (tv-model tl-widget) model)
      (set! (tv-sel tl-widget) selection))
    (ktlw/add-columns tl-widget treeview)
    tl-widget))


;;;
;;; Later to be generalized
;;;

(define (ktlw/build-filter-conditions-sepxr tl-widget)
  (let* ((date-f (gtk2/get-text (filter-date-entry tl-widget) 'trim))
	 (who-f (str/prep-str-for-sql (gtk2/get-text (filter-who-entry tl-widget) 'trim)))
	 (for-whom-f (str/prep-str-for-sql (gtk2/get-text (filter-for-whom-entry tl-widget) 'trim)))
	 (what-f (str/prep-str-for-sql (gtk2/get-text (filter-what-entry tl-widget) 'trim)))
	 (description-f (str/prep-str-for-sql (gtk2/get-text (filter-description-entry tl-widget) 'trim)))
	 (to-be-charged-f (ktlw/filter-to-be-charged? tl-widget)))
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
	       ("to_be_charged" text ,(ktlw/filter-to-be-charged-sqlvalue tl-widget)))
	     `(("date_" date ,date-f)
	       ("who" text ,who-f)
	       ("for_whom" text ,for-whom-f)
	       ("what" text ,what-f)
	       ("description" text ,description-f))))))

(define (ktlw/filter-to-be-charged? tl-widget)
  (case (get-active (filter-to-be-charged-combo tl-widget))
    ((0) #f) ;; it is not used as a filter criteria
    ((1) "to_be_charged = 't'") ;; used as a filter criteria and must be true
    ((2) "to_be_charged = 't'"))) ;; used as a filter criteria and must be false

(define (ktlw/filter-to-be-charged-sqlvalue tl-widget)
  (case (get-active (filter-to-be-charged-combo tl-widget))
    ((1) "t")
    ((2) "f")
    (else
     (format #t "ktlw/filter-to-be-charged-sqlvalue should not have been called!~%")
     #f)))

(define (ktlw/filter-to-be-charged-value tl-widget)
  (case (get-active (filter-to-be-charged-combo tl-widget))
    ((1) #t)
    ((2) #f)
    (else
     (format #t "ktlw/filter-to-be-charged-value should not have been called!~%")
     #f)))

(define (ktlw/update-status-bar-1 tl-widget)
  (let ((status-bar (status-bar-1 tl-widget))
	(new-row (current-row tl-widget))
	(tuples-nb (and (db-tuples tl-widget)
			(length (db-tuples tl-widget)))))
    (gtk2/status-pop status-bar "")
    (if (and tuples-nb (> tuples-nb 0))
	(gtk2/status-push status-bar (format #f "Record: ~A/~A" (1+ new-row) tuples-nb) "")
	(gtk2/status-push status-bar "Record: none" ""))))

(define (ktlw/select-ctime tl-widget)
  ;; Kise not only allows the user to define and activate a filter,
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

  ;; So, in order to be able to reuse the same db-kise/select-some
  ;; function we rebuild the where clause here.

  (let* ((filter? (active-filter tl-widget))
	 (id-set? (id-set tl-widget))
	 (f-tbc? (ktlw/filter-to-be-charged? tl-widget))
	 (f-tbc-true? (and f-tbc? (ktlw/filter-to-be-charged-value tl-widget)))
	 (clause (cond ((not filter?) "to_be_charged = 't'")
		       ((not id-set?)
			(if f-tbc-true?
			    filter?
			    (if (and f-tbc? (not f-tbc-true?))
				#f
				(format #f "(~A) and to_be_charged = 't'" filter?))))
		       (id-set?
			(let* ((filter-conditions (ktlw/build-filter-conditions-sepxr tl-widget))
			       (new-filter (and filter-conditions (dbf/get-filter filter-conditions))))
			  (format #f "((~A) or id in ~A) and to_be_charged = 't'"
				  new-filter
				  (dbf/build-set-expression id-set?))))
		       (else
			(format #t "did I missed something? ktlw/select-ctime uncoverd condition...~%")
			#f))))
    (if clause
	(begin
	  ;; (format #t "select-ctime: ~S~%" clause)
	  (db-kise/select-some clause #f "sum(duration)"))
	;; no clause can always return (#(#f)) which is
	;; what db-kise/select-some would return
	;; anyway
	(list (make-vector 1 #f)))))

(define (ktlw/get-hour-hours nb)
  (if (or (fp/<? nb 1) (fp/=? nb 1)) (_ "hour") (_ "hours")))

(define (ktlw/get-day-days nb)
  (if (or (fp/<? nb 1) (fp/=? nb 1)) (_ "day") (_ "days")))

(define (ktlw/get-totals tl-widget)
  (let* ((tresult (db-kise/select-some (active-filter tl-widget) (id-set tl-widget) "sum(duration)"))
	 ;; guile-sqlite returns a list of vectors
	 (ttime (vector-ref (car tresult) 0))
	 (tdays (and ttime (fp/round (/ ttime 8) 1)))
	 (cresult (ktlw/select-ctime tl-widget))
	 ;; guile-sqlite will return a list of 1 vector of 1
	 ;; value, which might be #f [aggregate of an empty set]
	 (ctime (vector-ref (car cresult) 0))
	 (cdays (and ctime (fp/round (/ ctime 8) 1))))
    (values (if ttime ttime 0) (if tdays tdays 0.0)
	    (if ctime ctime 0) (if cdays cdays 0.0))))

(define (ktlw/update-totals-status-bars tl-widget)
  (let ((ttime-sbar (status-bar-3 tl-widget)) ;; totals ...
	(ctime-sbar (status-bar-4 tl-widget))) ;; charged ...
    (gtk2/status-pop ttime-sbar "")
    (gtk2/status-pop ctime-sbar "")
    (if (null? (db-tuples tl-widget))
	(begin
	  (gtk2/status-push ttime-sbar
			    (format #f "~A : n/a" (_ "Total time"))
			    "")
	  (gtk2/status-push ctime-sbar 
			    (format #f "~A : n/a" (_ "Charged time"))
			    ""))
	(receive (ttime tdays ctime cdays)
	    (ktlw/get-totals tl-widget)
	  ;; here we will use ngettext
	  (gtk2/status-push ttime-sbar (format #f "~A: ~A ~A - ~A ~A"
					       (_ "Total time")
					       ttime (ktlw/get-hour-hours ttime)
					       tdays (ktlw/get-day-days tdays)) "")
	  (if ctime
	      (gtk2/status-push ctime-sbar (format #f "~A: ~A ~A - ~A ~A"
						   (_ "Charged time")
						   ctime (ktlw/get-hour-hours ctime)
						   cdays (ktlw/get-day-days cdays)) "")
	      (gtk2/status-push ctime-sbar "Charged time: n/a" ""))))))

(define (ktlw/select-row tl-widget new-row . noscroll?)
  (when (gui-callback? tl-widget)
    (let ((path (list new-row))
	  (tvsel (tv-sel tl-widget)))
      (set! (gui-callback? tl-widget) #f)
      ;; (format #t "Before unselect all~%")
      (unselect-all tvsel)
      ;; (format #t "Before select-path, new-row: ~S~%" new-row)
      (select-path tvsel path)
      (ktlw/update-status-bar-1 tl-widget)
      (when (null? noscroll?)
	(scroll-to-cell (tv tl-widget) path #f #t 0.3))
      (set! (gui-callback? tl-widget) #t))))

(define (ktlw/update-store-check-position tl-widget what new-value)
  ;; ATTENTION: il faut chercher l'iter courrant et non prendre
  ;;            la selection parce qu'il se peut que le présent appel
  ;;            résulte d'un focus-out-event provoquer par
  ;;            enregistrer-saisie-courante, ce qui signifie alors
  ;;            que la sélection a déjà changé, alors que
  ;;            current-row est toujours bon,
  ;;            c'est à dire qu'elle pointe le row qui
  ;;            était sélectionné 'juste avant'
  (let* ((old-pos (current-row tl-widget))
	 (db-tuple (and old-pos (ktlw/get-tuple tl-widget old-pos)))
	 (reference (db-kise/get db-tuple 'id))
	 (old-entry (and db-tuple (ktlw/get what tl-widget old-pos)))
	 (tuples (db-kise/select-some (active-filter tl-widget) (id-set tl-widget)))
	 (new-pos (db-kise/find-pos tuples 'id reference =))
	 (reordered? (not (= old-pos new-pos)))
	 (prev-gui-cb? (gui-callback? tl-widget))
	 (model (tv-model tl-widget))
	 (old-iter (get-iter model (list old-pos))))
    (kiseiter/set what model old-iter new-value)
    (when reordered?
      ;; we still need to solve things related 'filters'
      ;; if there is an active filter off course
      (let ((new-path (list new-pos))
	    (new-tuple (list-ref tuples new-pos)))
	(db-kise/set new-tuple what new-value)
	(set! (db-tuples tl-widget) tuples)
	(set! (gui-callback? tl-widget) #t)
	(if (< new-pos old-pos)
	    (move-before model
			 old-iter
			 (get-iter model new-path))
	    (move-after model
			old-iter
			(get-iter model new-path)))
	(ktlw/select-row tl-widget new-pos)
	;; (format #t "Old pos: ~A, New pos: ~A, Current row: ~A~%" old-pos new-pos (current-row tl-widget))
	(set! (gui-callback? tl-widget) prev-gui-cb?)))))

(define (ktlw/entry-std-cb entry tl-widget what list-store-col-pos empty-msg . date?)
  (let* ((old-pos (current-row tl-widget))
	 (db-tuple (and old-pos 
			(>= old-pos 0)
			(ktlw/get-tuple tl-widget old-pos)))
	 (reference (and db-tuple (db-kise/get db-tuple 'id)))
	 (old-entry (and db-tuple (ktlw/get what tl-widget old-pos)))
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
	    (receive (updated?)
		(if (null? date?)
		    (db-kise/update db-tuple what new-entry)
		    (if (date/valid-date? new-entry)
			(db-kise/update db-tuple what (date/iso-date new-entry) new-entry)
			#f))
	      (gtk2/status-pop (status-bar-2 tl-widget) "")
	      (if updated?
		  (begin
		    (when (active-filter tl-widget)
		      (ktlw/add-id reference tl-widget))
		    (when list-store-col-pos
		      (ktlw/update-store-check-position tl-widget what new-entry)))
		  (begin
		    (gdk-beep)
		    (gtk2/status-pop (status-bar-2 tl-widget) "")
		    (gtk2/status-push (status-bar-2 tl-widget) "Invalid date. The previous value is restored" "")
		    (gtk2/set-text entry old-entry)
		    )))))
    ;; must return #f, this is imposed by gtk+ 2.x
    #f))


;;;
;;; Open DB
;;;

(define (ktlw/create-db-checks db-file)
  ;; we need write access on the basename
  (let ((directory (dirname db-file)))
    (cond ((access? db-file F_OK) 'exists)
	  ((not (access? directory W_OK)) 'wrong-perm)
	  (else 'ok))))

(define (ktlw/open-db-checks db-file)
  ;; this will be called by kc/connect 'open mode or ktlw/open-db
  ;; based on kise.config infos. in both of these cases, it must
  ;; check that:
  ;;   a. the file [still] exists;
  ;;   b. that it is W_OK;
  ;;   c. that it is an sqlite db;
  ;;   d. which does have the kise schema
  (cond ((not (access? db-file F_OK)) 'does-not-exist)
	((not (access? db-file W_OK)) 'wrong-perm)
	((not (sqlite/sqlite-db-file? db-file)) 'not-an-sqlite-file)
	(else
	 (let* ((db (db-con/open db-file))
		(schema? (db/check-schema)))
	   (case schema?
	     ((complete) 'opened)
	     ((partial) 'opened-partial-schema)
	     ((none) 'opened-no-schema))))))

(define (ktlw/post-open-db-ops tl-widget db-fname) 
  ;; the list-store related operations that needs to be done @
  ;; connection time is exactly what needs to be done when clearing a
  ;; filter + filling the combos [obviously].
  (set! (active-filter tl-widget) #t)
  (set! (db-file tl-widget) db-fname)
  (set-markup (db-name-lb1 tl-widget)
	      (format #f "<span foreground=\"#777777\"><b>[ ~A ]</b></span>" (basename db-fname)))
  (ktlw/filter-clear tl-widget 'fillcombos))

(define (ktlw/open-db tl-widget db-file from-gui? mode open-at-startup? checks-result)
  (when from-gui?
    (sys/write-config "kise"
		      (list (cons 'db-file db-file)
			    (cons 'open-at-startup open-at-startup?)
			    (cons 'ulogo "/usr/lpdi/logos/micdigi")
			    )))
  (case mode
    ((open)
     (case checks-result
       ((opened) (ktlw/post-open-db-ops tl-widget db-file))
       ((opened-partial-schema)
	(md2b/select-gui (dialog tl-widget)
			 "Confirm dialog"
			 "Complete schema"
			 (_ "This database has an incomplete Kisê schema, would you like to complete it now?")
			 (lambda ()
			   (db/complete-schema)
			   (ktlw/post-open-db-ops tl-widget db-file))
			 (lambda () 'nothing)))
       ((opened-no-schema)
	(md2b/select-gui (dialog tl-widget)
			 "Confirm dialog"
			 "Add schema"
			 (_ "This database does not contain the Kisê schema, would you like to add it now?")
			 (lambda ()
			   (db/add-schema)
			   (ktlw/post-open-db-ops tl-widget db-file))
			 (lambda () 'nothing)))))
    ((create)
     ;; (format #t "ktlw/open-db: ~S ~S~%" mode db-file)
     (db-con/open db-file)
     (db/add-schema)
     (ktlw/post-open-db-ops tl-widget db-file))))


;;;
;;; Combos related
;;;

(define (ktlw/fill-combo tl-widget values-acc combo-acc db-field)
  (let ((values (db-kise/select-distinct db-field 'add-empty)))
    (set! (values-acc tl-widget) values)
    (gtk2/fill-combo (combo-acc tl-widget) values)))

(define (ktlw/fill-combos tl-widget)
  (for-each (lambda (vcd)
	       (ktlw/fill-combo tl-widget (car vcd) (cadr vcd) (caddr vcd)))
      `((,whos ,who-combo who)
	(,for-whoms ,for-whom-combo for_whom)
	(,whats ,what-combo what))))

(define (ktlw/connect-combos-1 combos-defs)
  (for-each (lambda (combo-def)
	      (let ((tl-widget (list-ref combo-def 0))
		    (which-combo (list-ref combo-def 1))
		    (which-entry (list-ref combo-def 2))
		    (db-fname (list-ref combo-def 3))
		    (in-store? (list-ref combo-def 4))
		    (kw-acc (list-ref combo-def 5))
		    (db-get-values-func (list-ref combo-def 6)))
		;; (format #t "~S, ~S, ~S~%" combo-def (which-combo tl-widget) (which-entry tl-widget))
		(connect (which-combo tl-widget)
			 'changed
			 (lambda (combo)
			   (when (gui-callback? tl-widget)
			     (let ((model (tv-model tl-widget))
				   (row (current-row tl-widget))
				   (iter (current-iter tl-widget))
				   (value (get-text (which-entry tl-widget))))
			       ;; (format #t "Row: ~S, iter-acc: ~S, combo: ~S 'changed: ~S~%" row kw-acc which-combo value)
			       ;; update-db
			       (ktlw/set db-fname tl-widget value row)
			       ;; if active-filter, we add the id to the set, even if we are not
			       ;; certain this is absolutely necessary,since the cost of checking would
			       ;; actually be much much higher.
			       (when (active-filter tl-widget)
				 (let ((tuple (ktlw/get-tuple tl-widget row)))
				   (ktlw/add-id (db-kise/get tuple 'id) tl-widget)))
			       ;; update store
			       (when in-store?
				 (ktlw/update-store-check-position tl-widget db-fname value))
			       ))))
		(connect (which-combo tl-widget)
			 'popup
			 (lambda (combo)
			   (let ((value (get-text (which-entry tl-widget)))
				 (new-values (db-get-values-func)))
			     (set! (kw-acc tl-widget) new-values)
			     (gtk2/fill-combo combo new-values)
			     (set-active combo (gtk2/combo-find-row combo value)))))
		(connect (which-entry tl-widget)
			 'focus-out-event
			 (lambda (entry event)
			   (when (gui-callback? tl-widget)
			     (let* ((combo (which-combo tl-widget))
				    ;; 'changed combo callback has already been executed
				    ;; -> the new-value is already in the DB
				    (new-value (get-text entry))
				    (new-values (db-get-values-func)))
			       ;; (format #t "Row: ~S, ~S: new value: ~S, ~S - new values: ~S~%" 
			       ;;     (current-row tl-widget) combo new-value kw-acc new-values)
			       (set! (gui-callback? tl-widget) #f)
			       (set! (kw-acc tl-widget) new-values)
			       (gtk2/fill-combo combo new-values)
			       (set-active combo (gtk2/combo-find-row combo new-value))
			       (set! (gui-callback? tl-widget) #t)))
			   ;; gtk2 requirement ...
			   #f))))
      combos-defs))

(define (ktlw/connect-combos tl-widget)
  (ktlw/connect-combos-1 `((,tl-widget ,who-combo ,who-entry who #t ,whos 
				       ,(lambda () (db-kise/select-distinct 'who #t)))
			   (,tl-widget ,for-whom-combo ,for-whom-entry for_whom #t ,for-whoms 
				       ,(lambda () (db-kise/select-distinct 'for_whom #t)))
			   (,tl-widget ,what-combo ,what-entry what #t ,whats 
				       ,(lambda () (db-kise/select-distinct 'what #t))))))


;;;
;;; API
;;;

(define (ktlw/get-tuple tl-widget row)
  (list-ref (db-tuples tl-widget) row))

(define (ktlw/get what tl-widget . row)
  (let* ((which-row (if (null? row) (current-row tl-widget) (car row)))
	 (db-tuple (ktlw/get-tuple tl-widget which-row)))
    ;; (format #t "ktlw/get: row: ~S, what: ~S, tuple: ~S~%" which-row what db-tuple)
    (db-kise/get db-tuple what)))

(define (ktlw/set what tl-widget value . row)
  (let* ((which-row (if (null? row) (current-row tl-widget) (car row)))
	 (db-tuple (ktlw/get-tuple tl-widget which-row)))
    ;; (format #t "~S~%" db-tuple)
    (db-kise/update db-tuple what value)))

(define (ktlw/fill-tv tl-widget)
  ;; (format #t "ktlw/fill-tv is been executed...~%")
  (let ((model (tv-model tl-widget)))
    (gtk-list-store-clear model)
    (for-each (lambda (tuple)
		;; (format #t "~S~%" tuple)
		(kiseiter/append-fill model
				      (db-kise/get tuple 'date_)
				      (db-kise/get tuple 'who)
				      (db-kise/get tuple 'for_whom)
				      ;; (commify (vector-ref tuple 4) 1) ;; duration
				      (db-kise/get tuple 'duration)
				      (sqlite/true? (db-kise/get tuple 'to_be_charged))
				      (db-kise/get tuple 'what)
				      ))
	(db-tuples tl-widget))))

(define (ktlw/make-tl-widget uname gfile)
  (let* ((xmlc (glade-xml-new gfile  #f "kise"))
	 (t-tip (gtk-tooltips-new))
	 (tl-widget (make <kise/tl-widget>
		      :user-name uname
		      :glade-file gfile
		      :xml-code xmlc
		      :dialog (get-widget xmlc "kise")
		      :menubar (get-widget xmlc "kise/menubar")
		      :tooltip t-tip
		      :sorting-lb (get-widget xmlc "kise/sorting_lb")
		      :sorting-combo (get-widget xmlc "kise/sorting_combo")

		      :con-bt (get-widget xmlc "kise/con_bt")
		      :dup-bt (get-widget xmlc "kise/dup_bt")
		      :add-bt (get-widget xmlc "kise/add_bt")
		      :del-bt (get-widget xmlc "kise/del_bt")
		      :print-bt (get-widget xmlc "kise/print_bt")

		      :first-bt (get-widget xmlc "kise/first_bt")
		      :prev-bt (get-widget xmlc "kise/prev_bt")
		      :next-bt (get-widget xmlc "kise/next_bt")
		      :last-bt (get-widget xmlc "kise/last_bt")
		      ;; :help-bt (get-widget xmlc "kise/help_bt")
		      :reference-lb (get-widget xmlc "kise/reference_lb")
		      :reference-entry (get-widget xmlc "kise/reference_entry")
		      :date-lb (get-widget xmlc "kise/date_lb")
		      :date-entry (get-widget xmlc "kise/date_entry")
		      :date-icon (get-widget xmlc "kise/date_icon")

		      :who-lb (get-widget xmlc "kise/who_lb")
		      :who-combo (get-widget xmlc "kise/who_combo")
		      :who-entry (gtk-bin-get-child (get-widget xmlc "kise/who_combo"))

		      :for-whom-lb (get-widget xmlc "kise/for_whom_lb")
		      :for-whom-combo (get-widget xmlc "kise/for_whom_combo")
		      :for-whom-entry (gtk-bin-get-child (get-widget xmlc "kise/for_whom_combo"))

		      :duration-lb (get-widget xmlc "kise/duration_lb")
		      :duration-sb (get-widget xmlc "kise/duration_sb")

		      :what-lb (get-widget xmlc "kise/what_lb")
		      :what-combo (get-widget xmlc "kise/what_combo")
		      :what-entry (gtk-bin-get-child (get-widget xmlc "kise/what_combo"))
		      ;;:what-tv (get-widget xmlc "kise/what_tv")

		      :to-be-charged-cb (get-widget xmlc "kise/to_be_charged_cb")
		      :description-lb (get-widget xmlc "kise/description_lb")
		      :description-entry (get-widget xmlc "kise/description_entry")

		      :db-name-lb1 (get-widget xmlc "kise/db_name_lb1")
		      :db-name-lb2 (get-widget xmlc "kise/db_name_lb2")
		      :db-name-entry (get-widget xmlc "kise/db_name_entry")

		      :filter-apply-bt (get-widget xmlc "kise/filter_apply_bt")
		      :filter-clear-bt (get-widget xmlc "kise/filter_clear_bt")
		      :filter-select-bt (get-widget xmlc "kise/filter_select_bt")
		      
		      :filter-date-entry (get-widget xmlc "kise/filter_date")
		      :filter-who-entry (get-widget xmlc "kise/filter_who")
		      :filter-for-whom-entry (get-widget xmlc "kise/filter_for_whom")
		      :filter-what-entry (get-widget xmlc "kise/filter_what")
		      :filter-description-entry (get-widget xmlc "kise/filter_description")
		      :filter-to-be-charged-cb (get-widget xmlc "kise/filter_to_be_charged_cb")
		      :filter-to-be-charged-eb (get-widget xmlc "kise/filter_to_be_charged_eb")
		      :filter-to-be-charged-combo (get-widget xmlc "kise/filter_to_be_charged_combo")
		      
		      :tv (get-widget xmlc "kise/tv")
		      :status-bar-1 (get-widget xmlc "kise/status_bar_1")
		      :status-bar-2 (get-widget xmlc "kise/status_bar_2")
		      :status-bar-3 (get-widget xmlc "kise/status_bar_3")
		      :status-bar-4 (get-widget xmlc "kise/status_bar_4"))))
    (ktlw/setup-treeview tl-widget)
    ;; the combos need to be cleared since some example items
    ;; are defined in the glade file
    (gtk2/clear-combo (who-combo tl-widget))
    (gtk2/clear-combo (for-whom-combo tl-widget))
    (gtk2/clear-combo (what-combo tl-widget))
    (ktlw/connect-combos tl-widget)
    (set-sensitive (filter-select-bt tl-widget) #f)
    ;; not yet but soon
    (ktlw/translate tl-widget)
    tl-widget))


;;;
;;; Adding rows
;;;

(define (ktlw/add tl-widget)
  (let* ((model (tv-model tl-widget))
	 (today (date/system-date))
	 (iso-today (date/iso-date today))
	 (uname (user-name tl-widget))
	 (filter? (active-filter tl-widget))
	 (restore-mode? (null? (db-tuples tl-widget)))
	 (new-id (db-kise/add iso-today
			      uname	;; who
			      ""	;; for_whom
			      "" 	;; what 
			      0		;; duration
			      "f"	;; to-be-charged
			      ""	;; description
			      ))
	 (ids? (if filter? (ktlw/add-id new-id tl-widget) (id-set tl-widget)))
	 (new-iter (kiseiter/prepend-fill model today uname "" 0 #f ""))
	 (tuples (db-kise/select-some filter? ids?))
	 (new-pos (db-kise/find-pos tuples 'id new-id =)))
    (set! (db-tuples tl-widget) tuples)
    (unless (sqlite/pos uname (whos tl-widget) string=? 0)
      (let ((prev-gui-cb? (gui-callback? tl-widget))
	    (w-combo (who-combo tl-widget)))
	(set! (gui-callback? tl-widget) #f)
	(ktlw/fill-combo tl-widget whos who-combo 'who)
	(set-active w-combo (gtk2/combo-find-row w-combo uname))
	(set! (gui-callback? tl-widget) prev-gui-cb?)))
    (when restore-mode? (ktlw/normal-mode tl-widget))
    (if (= new-pos 0)
	(ktlw/select-row tl-widget 0)
	(begin
	  (move-after model
		      new-iter
		      (get-iter model (list new-pos)))
	  (ktlw/select-row tl-widget new-pos)))))

(define (ktlw/duplicate tl-widget)
  (let* ((model (tv-model tl-widget))
	 (row (current-row tl-widget))
	 (iter (current-iter tl-widget))
	 (tuple (ktlw/get-tuple tl-widget row))
	 (new-id (db-kise/duplicate (db-kise/get tuple 'id) tuple))
	 (tuples (db-kise/select-some (active-filter tl-widget) (id-set tl-widget)))
	 (new-pos (db-kise/find-pos tuples 'id new-id =))
	 (new-iter (kiseiter/prepend-fill model
					  (kiseiter/get 'date model iter)
					  (kiseiter/get 'who model iter)
					  (kiseiter/get 'for-whom model iter)
					  (kiseiter/get 'duration model iter)
					  (kiseiter/get 'to-be-charged model iter)
					  (kiseiter/get 'what model iter))))
    ;; (format #t "ktlw/duplicate - new-pos: ~S~%" new-pos)
    (set! (db-tuples tl-widget) tuples)
    (move-before model new-iter iter)
    (ktlw/select-row tl-widget new-pos)
    (ktlw/update-totals-status-bars tl-widget)))

(define (ktlw/delete-msg-str)
  ;; with "~10,,,' @A" it would be right justified but because this is
  ;; passed to a gtk label widget, which uses variable size font, it
  ;; is not sufficient and not as nice at it should.
  "Are you sure you want to delete this entry ?

	~10,,,' A: ~A
	~10,,,' A: ~A
	~10,,,' A: ~A
	~10,,,' A: ~A")

(define (ktlw/delete tl-widget)
  (let* ((model (tv-model tl-widget))
	 (row (current-row tl-widget))
	 (last-row (1- (gtk-tree-model-iter-n-children model #f)))
	 (iter (current-iter tl-widget))
	 (tuple (ktlw/get-tuple tl-widget row))
	 (reference (db-kise/get tuple 'id)))
    (md2b/select-gui (dialog tl-widget)
		     "Confirm dialog"
		     "Deletion"
		     (format #f "~?" (ktlw/delete-msg-str)
			     (list "Reference" reference
				   "Date" (db-kise/get tuple 'date_)
				   "Who" (db-kise/get tuple 'who)
				   "for whom" (db-kise/get tuple 'for_whom)))
		     (lambda ()
		       (db-kise/delete reference)
		       (let* ((a-filter? (active-filter tl-widget))
			      (new-id-set (and a-filter? (ktlw/del-id reference tl-widget)))
			      (tuples (db-kise/select-some a-filter? new-id-set))
			      (its-length (length tuples)))
			 (set! (db-tuples tl-widget) tuples)
			 (remove model iter)
			 (if (> its-length 0)
			     (if (= row last-row)
				 (ktlw/select-row tl-widget (1- last-row) 'noscroll)
				 (ktlw/select-row tl-widget row 'noscroll))
			     (ktlw/empty-subset-or-db-mode tl-widget))
			 (ktlw/update-totals-status-bars tl-widget)))
		     (lambda () 'nothing))))


;;;
;;; Other GUI related
;;;

(define (ktlw/check-nav-tb-sensitive-needs tl-widget line-nb)
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

(define (ktlw/empty-subset-or-db-mode tl-widget)
  (let ((prev-gui-cb? (gui-callback? tl-widget)))
    (set! (gui-callback? tl-widget) #f)
    (set! (current-row tl-widget) -1)
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
	`(,add-bt))
    (for-each (lambda (acc)
		;; (format #t "empty-mode: ~S~%" acc)
		(set-sensitive (acc tl-widget) #f))
	`(,print-bt
	  ,first-bt ,prev-bt ,next-bt ,last-bt ;; (ktlw/check-nav-tb-sensitive-needs tl-widget -1)
	  ,dup-bt ,del-bt
	  ,date-entry
	  ,description-entry
	  ,who-combo
	  ,for-whom-combo
	  ,what-combo
	  ,duration-sb
	  ,to-be-charged-cb
	  ,tv))
    (ktlw/update-status-bar-1 tl-widget)))

(define (ktlw/no-db-mode tl-widget)
  (ktlw/empty-subset-or-db-mode tl-widget)
  (for-each (lambda (acc)
	      (set-sensitive (acc tl-widget) #f))
      `(,add-bt
	,filter-apply-bt
	,filter-clear-bt
	,filter-select-bt
	,filter-date-entry
	,filter-who-entry
	,filter-for-whom-entry
	,filter-what-entry
	,filter-description-entry
	,filter-to-be-charged-combo)))

(define (ktlw/normal-mode tl-widget)
  (for-each (lambda (acc)
	      (set-sensitive (acc tl-widget) #t))
      `(,print-bt
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

(define (ktlw/filter-get-row-new-pos-if-any tl-widget new-tuple-set)
  (let* ((row (current-row tl-widget))
	 (tuple (and row (>= row 0) (ktlw/get-tuple tl-widget row)))
	 (reference (and tuple (db-kise/get tuple 'id))))
    (and reference
	 (db-kise/find-pos new-tuple-set 'id reference =))))

(define (ktlw/filter-apply tl-widget . force?)
  (format #t "applying filter: ~S~%" force?)
  (let* ((prev-filter (active-filter tl-widget))
	 (filter-conditions? (ktlw/build-filter-conditions-sepxr tl-widget))
	 (filter? (and filter-conditions? (dbf/get-filter filter-conditions?))))
    ;; being invalid, the date only may lead to filter? being #f
    (if filter?
	(if (or (not prev-filter)
		(not (null? force?))
		(not (string=? filter? prev-filter)))
	    ;; in this case we reset the id-set to #f
	    (begin
	      (set! (active-filter tl-widget) filter?)
	      (set! (id-set tl-widget) #f)
	      (let* ((new-tuple-set (db-kise/select-some filter? #f))
		     (new-pos (ktlw/filter-get-row-new-pos-if-any tl-widget new-tuple-set)))
		(format #t "filter is: ~S~%new-set: ~S, new-pos: ~S~%" filter? (length new-tuple-set) new-pos)
		(set! (db-tuples tl-widget) new-tuple-set)
		(ktlw/fill-tv tl-widget)
		(ktlw/update-totals-status-bars tl-widget)
		(if (null? new-tuple-set)
		    (ktlw/empty-subset-or-db-mode tl-widget)
		    (begin
		      (ktlw/normal-mode tl-widget)
		      (if new-pos
			  (ktlw/select-row tl-widget new-pos)
			  (ktlw/select-row tl-widget 0)))))))
	(begin
	  (when prev-filter
	    (ktlw/filter-clear tl-widget))
	  (format #t "filter is empty~%")
	  ))))

(define (ktlw/filter-clear tl-widget . fillcombos?)
  ;; (format #t "clearing filter: ~S~%" fillcombos?)
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
    (let* ((new-tuple-set (db-kise/select-all))
	   (new-pos (ktlw/filter-get-row-new-pos-if-any tl-widget new-tuple-set)))
      (set! (db-tuples tl-widget) new-tuple-set)
      (if (not (null? fillcombos?)) (ktlw/fill-combos tl-widget))
      (ktlw/fill-tv tl-widget)
      (if (null? new-tuple-set)
	  (ktlw/empty-subset-or-db-mode tl-widget)
	  (begin
	    (ktlw/normal-mode tl-widget)
	    (set! (gui-callback? tl-widget) #t)	    
	    (if new-pos
		(ktlw/select-row tl-widget new-pos)
		(ktlw/select-row tl-widget 0))))))
  (set! (gui-callback? tl-widget) #t)
  ;; this has to be last
  (ktlw/update-totals-status-bars tl-widget))

;;;
;;; Tooltips and dialog translation
;;;

(define (ktlw/translate tl-widget)
  (let ((t-tip (tooltip tl-widget)))
    #;(set-icon-from-stock (date-entry tl-widget)
			 GTK_ENTRY_ICON_SECONDARY
			 GTK_STOCK_ABOUT)
    #;(set-markup (date-lb tl-widget) "<a href=\"www.fsf.org\">Date:</a>")
    #;(set-tooltip (date-icon tl-widget) t-tip
		 (_ "Dates:

In its current version [1], Kisê only support the following date format: dd<sep>mm<sep>yyyy

	<sep> can be '.', '/' or '-' providing it's used consistently [twice the same seperator in a date].

[1]	support for locale will be provided in the future") "")
    #f))


#!

(use-modules (kise tl-widget))
(reload-module (resolve-module '(kise tl-widget)))


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


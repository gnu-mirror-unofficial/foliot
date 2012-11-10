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

(define-module (kise kise)
  ;; guile/guile-gnome
  :use-module (ice-9 format)
  :use-module (ice-9 receive)
  :use-module (oop goops)
  :use-module (gnome gnome) ;; could [later] use the help system
  :use-module (gnome gobject)
  :use-module (gnome gtk)

  ;; common
  :use-module (macros reexport)
  :use-module (macros push)
  :use-module (macros do)
  :use-module (system dates)
  :use-module (system i18n)
  :use-module (db sqlite)
  :use-module (nbs all)
  :use-module (gtk all)

  ;; kise
  :use-module (kise db)
  :use-module (kise config)
  :use-module (kise colours)
  :use-module (kise globals)
  :use-module (kise iter)
  :use-module (kise tl-widget)
  :use-module (kise connect)
  :use-module (kise print)

  :duplicates (merge-generics 
	       replace
	       warn-override-core
	       warn
	       last)

  :export (*tl-widget*
	   kise/animate-ui
	   kise/set-debug-variables ;; debug mode
	   tl-widget
	   model
	   treeview
	   selection
	   row
	   iter
	   tuple))


(eval-when (compile load eval)
  (re-export-public-interface (oop goops)
			      (gnome gobject)
			      (gnome gtk)
			      ;; common
			      (system dates)
			      (system i18n)
			      (db sqlite)
			      (nbs all)
			      (gtk all)
			      ;; kise
			      (kise db)
			      (kise config)
			      (kise colours)
			      (kise globals)
			      (kise iter)
			      (kise tl-widget)
			      (kise connect)
			      (kise print))
  (textdomain "kise")
  (bindtextdomain "kise" (aglobs/get 'pofdir)))


;;;
;;; Globals
;;;

(define *tl-widget* #f)

;; debug 'mode'
(define tl-widget #f)
(define model #f)
(define treeview #f)
(define selection #f)
(define row #f)
(define iter #f)
(define tuple #f)

(define (kise/set-debug-variables)
  (set! tl-widget *tl-widget*)
  (set! treeview (tv tl-widget))
  (set! model (tv-model tl-widget))
  (set! selection (tv-sel tl-widget))
  (set! row (current-row tl-widget))
  (set! iter (current-iter tl-widget))
  (set! tuple (ktlw/get-tuple tl-widget row)))

#!

(use-modules (kise kise))
(kise/set-debug-variables)

(define acti-combo (what-combo tl-widget))
(define acti-entry (what-combo-entry tl-widget))

!#

;;;
;;; row selection related stuff
;;;


(define (kise/set-gtk-entries tl-widget row iter)
  (let ((tuple (ktlw/get-tuple tl-widget row))
	(model (tv-model tl-widget)))
    ;; (format #t "~S~%" tuple)
    (set-text (reference-entry tl-widget) (number->string (ktlw/get 'id tl-widget row)))
    (set-text (date-entry tl-widget) (kiseiter/get 'date model iter))
    (let ((who? (gtk2/combo-find-row (who-combo tl-widget) (db-kise/get tuple 'who))))
      (if who?
	  (set-active (who-combo tl-widget) who?)
	  (begin
	    ;; (gtk2/set-text (who-entry tl-widget) "")
	    (set-active (who-combo tl-widget) -1))))
    (let ((for-whom? (gtk2/combo-find-row (for-whom-combo tl-widget) (db-kise/get tuple 'for_whom))))
      (if for-whom?
	  (set-active (for-whom-combo tl-widget) for-whom?)
	  (begin
	    ;; (gtk2/set-text (for-whom-entry tl-widget) "")
	    (set-active (for-whom-combo tl-widget) -1))))
    (set-value (duration-sb tl-widget) (kiseiter/get 'duration model iter))
    (let ((what? (gtk2/combo-find-row (what-combo tl-widget) (db-kise/get tuple 'what))))
      (if what?
	  (set-active (what-combo tl-widget) what?)
	  (begin
	    ;; (gtk2/set-text (what-entry tl-widget) "")
	    (set-active (what-combo tl-widget) -1))))
    (gtk2/set-text (description-entry tl-widget) (ktlw/get 'description tl-widget row))
    (set-active (to-be-charged-cb tl-widget) (kiseiter/get 'to-be-charged model iter))))


;;;
;;; User interface
;;;

(define (kise/on-delete-window tl-widget)
  (kise/on-tv-row-change tl-widget)
  (exit 0))

(define (kise/on-tv-row-change tl-widget)
  ;; (format #t "On TV row change~%")
  (when (gui-callback? tl-widget)
    (let* ((main-window (dialog tl-widget))
	   (focusw (get-focus main-window))
	   (old-row  (current-row tl-widget)))
      ;; (format #t "~A focusw: ~A~%" (get-name main-window) (and focusw (get-name focusw)))
      (when (and old-row
		 (>= old-row 0) ;; -1 when no rows
		 focusw
		 (not (eq? focusw (tv tl-widget))))
	;; (format #t "  saving entries for row: ~A~%" old-row)
	(if (eq? focusw (description-entry tl-widget))
	    (set-focus main-window (date-entry tl-widget))
	    (set-focus main-window (description-entry tl-widget)))))))


;;;
;;; Animate GUI
;;;

(define (kise/open-db-cant-open-str)
  (_ "Some problem occured while trying to open your default Kisê database: ~A. It could be that the file has been deleted, moved, that it exists but you don't have 'write permission' over it, or that, for some reason you'll have to determine, it is not a Kisê database file anymore. Please check all of the above and start again or create/connect to another Kisê database."))

(define (kise/open-db tl-widget)
  (let ((db-file (kcfg/get 'db-file))
	(open-at-startup (kcfg/get 'open-at-startup)))
    (if (and db-file open-at-startup)
	;; we still need to proceed with all checks: it could have
	;; been deleted, moved, chmod, delted schema ...
	(let ((open-db-checks-result (ktlw/open-db-checks db-file)))
	  (case (ktlw/open-db-checks db-file)
	    ((does-not-exist wrong-perm not-an-sqlite-file)
	     (md1b/select-gui (dialog tl-widget)
			      (_ "Warning!")
			      (_ "DB connection problem:")
			      (format #f "~?" (kise/open-db-cant-open-str) (list db-file))
			      (lambda () (ktlw/no-db-mode tl-widget))))
	    ((opened opened-partial-schema opened-no-schema)
	     (ktlw/open-db tl-widget db-file #f 'open #f open-db-checks-result))))
	(begin
	  (ktlw/no-db-mode tl-widget)
	  (emit (con-bt tl-widget) 'clicked)))))

(define (kise/exit tl-widget)
  (md2b/select-gui (dialog tl-widget)
		   (_ "Exit")
		   (_ "Exit")
		   (_ "Exit Kisê ?")
		   (lambda () 
		     (kise/on-tv-row-change tl-widget)
		     (exit 0))
		   (lambda () 'nothing)))

(define (kise/animate-ui uname gfile version debug-mode)
  (let ((tl-widget (ktlw/make-tl-widget uname gfile)))
    ;; the config has already been red, now just call kcfg/get when
    ;; necessary or (kcgf/get 'reload) [if you manually change the
    ;; file for example for testing purposes]
    (set! *tl-widget* tl-widget)
    (connect (dialog tl-widget)
	     'delete-event
	     (lambda (. args)
	       (kise/exit tl-widget)
	       #t)) ;; stops the event to be propagated
    (connect (con-bt tl-widget)
	     'clicked
	     (lambda (button)
	       (kise/on-tv-row-change tl-widget)
	       (kc/select-gui tl-widget)))
    (connect (quit-bt tl-widget)
	     'clicked
	     (lambda (button) (kise/exit tl-widget)))
    (connect (dup-bt tl-widget)
	     'clicked
	     (lambda (button)
	       (kise/on-tv-row-change tl-widget)
	       (ktlw/duplicate tl-widget)))
    (connect (add-bt tl-widget)
	     'clicked
	     (lambda (button)
	       (kise/on-tv-row-change tl-widget)
	       (ktlw/add tl-widget)))
    (connect (del-bt tl-widget)
	     'clicked
	     (lambda (button)
	       (ktlw/delete tl-widget)))
    (connect (print-bt tl-widget)
	     'clicked
	     (lambda (button)
	       (kise/on-tv-row-change tl-widget)
	       (kp/select-gui tl-widget)))
    (connect (first-bt tl-widget)
	     'clicked
	     (lambda (button)
	       (ktlw/select-row tl-widget 0)))
    (connect (prev-bt tl-widget)
	     'clicked
	     (lambda (button)
	       (ktlw/select-row tl-widget (- (current-row tl-widget) 1))))
    (connect (next-bt tl-widget)
	     'clicked
	     (lambda (button)
	       (ktlw/select-row tl-widget (+ (current-row tl-widget) 1))))
    (connect (last-bt tl-widget)
	     'clicked
	     (lambda (button)
	       (ktlw/select-row tl-widget (- (length (db-tuples tl-widget)) 1))))
    #!
    (connect (help-bt tl-widget)
	     'clicked
	     (lambda (button)
	       (format #t (_ "this will call the online help~%"))))
    !#

    (connect (date-entry tl-widget)
	     'focus-out-event
	     (lambda (entry event)
	       ;; (format #t "entry ~S // event ~S~%" entry event)
	       (ktlw/entry-std-cb entry
				  tl-widget
				  'date_
				  0 ;; list-store offset or #f
				  ;; msg to display if empty and not allowed to be
				  (_ "The date can not be empty. It has been reset to its previous value.")
				  'date)))

    ;; what-tv connect ...
    ;; (kwt/connect-what-tree tl-widget)

    (connect (duration-sb tl-widget)
	     'value-changed
	     (lambda (widget)
	       (when (gui-callback? tl-widget)
		 (let ((model (tv-model tl-widget))
		       (row (current-row tl-widget))
		       (iter (current-iter tl-widget))
		       (value (get-value widget)))
		   ;; (dimfi row iter value)
		   (kiseiter/set 'duration model iter value)
		   ;; update-db
		   (ktlw/set 'duration tl-widget value row)
		   (ktlw/update-totals-status-bars tl-widget)))))

    (connect (to-be-charged-cb tl-widget)
	     'toggled
	     (lambda (widget)
	       (when (gui-callback? tl-widget)
		 ;; (dimfi "to-be-charged callback, gui-callback true...")
		 (let* ((model (tv-model tl-widget))
			(row (current-row tl-widget))
			(tuple (ktlw/get-tuple tl-widget row))
			(id (db-kise/get tuple 'id))
			(iter (current-iter tl-widget))
			(new-value (get-active widget)))
		   ;; do it on the toggle in the list store too ...
		   (when (active-filter tl-widget) (ktlw/add-id id tl-widget))
		   (set! (gui-callback? tl-widget) #f)
		   (kiseiter/set 'to-be-charged model iter new-value)
		   (set! (gui-callback? tl-widget) #t)
		   ;; update-db
		   (ktlw/set 'to_be_charged tl-widget (sqlite/boolean new-value) row)
		   (ktlw/update-totals-status-bars tl-widget)
		   (ktlw/update-store-check-position tl-widget 'to_be_charged new-value #f)))))

    (connect (description-entry tl-widget)
	     'focus-out-event
	     (lambda (entry event)
	       ;; (format #t "entry ~S // event ~S~%" entry event)
	       (when (gui-callback? tl-widget)
		 (ktlw/entry-std-cb entry
				    tl-widget
				    'description
				    #f ; column position if used in the list-store
				    #f ; msg to display if empty and not allowed to be
				    ))))

    ;;
    ;; filters
    ;;
    (connect (filter-apply-bt tl-widget)
	     'clicked
	     (lambda (button)
	       (kise/on-tv-row-change tl-widget)
	       (gtk2/status-pop (status-bar-2 tl-widget) "")
	       (ktlw/filter-apply tl-widget 'force)))

    (connect (filter-clear-bt tl-widget)
	     'clicked
	     (lambda (button)
	       (kise/on-tv-row-change tl-widget)
	       (gtk2/status-pop (status-bar-2 tl-widget) "")
	       (ktlw/filter-clear tl-widget)))

    (connect (filter-select-bt tl-widget)
	     'clicked
	     (lambda (button)
	       (kise/on-tv-row-change tl-widget)
	       (gtk2/status-pop (status-bar-2 tl-widget) "")
	       ;; 
	       ))

    (connect (filter-date-entry tl-widget)
	     'focus-out-event
	     (lambda (entry event)
	       (let* ((value (gtk2/get-text entry))
		      (trimed (string-trim-both value char-set:blank))
		      (empty? (string-null? trimed))
		      (filter? (and (not empty?)
				    (date/get-filter trimed))))
		 (if (not filter?)
		     (begin
		       (gdk-beep)
		       (gtk2/status-pop (status-bar-2 tl-widget) "")
		       (gtk2/status-push (status-bar-2 tl-widget) 
					 "Invalid date filter expression"
					 "")
		       (gtk2/set-text entry ""))
		     (gtk2/status-pop (status-bar-2 tl-widget) ""))
		 ;; we still have to call filter-apply of course
		 (ktlw/filter-apply tl-widget)
		 #f)))

    (connect (filter-who-entry tl-widget)
	     'focus-out-event
	     (lambda (entry event)
	       (gtk2/status-pop (status-bar-2 tl-widget) "")
	       (ktlw/filter-apply tl-widget)
	       #f))

    (connect (filter-for-whom-entry tl-widget)
	     'focus-out-event
	     (lambda (entry event)
	       (gtk2/status-pop (status-bar-2 tl-widget) "")
	       (ktlw/filter-apply tl-widget)
	       #f))

    (connect (filter-what-entry tl-widget)
	     'focus-out-event
	     (lambda (entry event)
	       (gtk2/status-pop (status-bar-2 tl-widget) "")
	       (ktlw/filter-apply tl-widget)
	       #f))

    (connect (filter-description-entry tl-widget)
	     'focus-out-event
	     (lambda (entry event)
	       (gtk2/status-pop (status-bar-2 tl-widget) "")
	       (ktlw/filter-apply tl-widget)
	       #f))

    (connect (filter-to-be-charged-combo tl-widget)
	     'changed
	     (lambda (combo)
	       (if (gui-callback? tl-widget)
		   (ktlw/filter-apply tl-widget 'force))))

    ;;
    ;; treeview
    ;; 
    (connect (tv tl-widget)
	     'row-activated
	     (lambda (tview path col)
	       (kise/on-tv-row-change tl-widget)
	       (let* ((model (get-model tview))
		      (iter  (get-iter model path)))
		 ;; (format #t "  ~S row activated~%" (car path))
		 #f)))

    (connect (tv-sel tl-widget)
	     'changed
	     (lambda (selection)
	       ;; (dimfi "row-changed " (g-reselect-path? tl-widget))
	       (kise/on-tv-row-change tl-widget)
	       (unless (g-reselect-path? tl-widget)
		 (receive (model iter)
		     (get-selected selection)
		   (if iter ;; is #f when filter-apply and prev row not in subset
		       (let* ((path (get-path model iter))
			      (row (car path))
			      (guicbpv? (gui-callback? tl-widget)))
			 ;; (dimfi "row-changed - new-row: " row)
			 (gtk2/status-pop (status-bar-2 tl-widget) "")
			 (ktlw/set-cur-globals tl-widget row iter)
			 (set! (gui-callback? tl-widget) #f)
			 (kise/set-gtk-entries tl-widget row iter)
			 (set! (gui-callback? tl-widget) guicbpv?)
			 (ktlw/check-nav-tb-sensitive-needs tl-widget (1+ row))
			 (ktlw/update-status-bar-1 tl-widget)))))))

    (connect (tv tl-widget)
	     'cursor-changed
	     (lambda (tview)
	       ;; this signal is last activated and could be used
	       ;; instead of the (g-reselect-path? tl-widget) 'trick'
	       (if (g-reselect-path? tl-widget)
		   (let ((tv-sel (tv-sel tl-widget))
			 (path (g-reselect-path? tl-widget)))
		     ;; (dimfi "cursor changed, g-reselect-path: " path)
		     (unselect-all tv-sel)
		     (set! (g-reselect-path? tl-widget) #f)
		     (select-path tv-sel path)))
	       #f))

    (gtk2/set-sensitive `(,(reference-entry tl-widget)) #f)
    (show-all (dialog tl-widget))
    (gtk2/hide `(,(menubar tl-widget)
		 ,(date-icon tl-widget)
		 ,(get-widget (xml-code tl-widget) "kise/order_tb")
		 ,(get-widget (xml-code tl-widget) "kise/db_name_bt")
		 ,(get-widget (xml-code tl-widget) "kise/nav_tb_2")
		 ,(db-name-lb2 tl-widget)
		 ,(db-name-lb3 tl-widget)))
    (kise/open-db tl-widget)))


#!

(use-modules (kise kise))
(reload-module (resolve-module '(kise kise)))


;;;
;;;
;;;

(kise/set-debug-variables)
(get-property-names <gtk-label>)
(get (filter-date tl-widget) 'width-request)
(get-allocation (description-sw tl-widget))
(get-allocation (filter-date-entry tl-widget))
(get-allocation (filter-to-be-charged-lb tl-widget))


;;;
;;; Colours [see Glade as well]
;;;

;; dark blue
<span foreground="#002F94"><b>Filtre</b></span>
;; some brown
<span foreground="#aa5500">Date:</span>
;; some brown less 'orange'
<span foreground="#91571c">Date:</span>


;;;
;;; Completion previous code
;;;

(connect (who-entry tl-widget)
	 'focus-in-event
	 (lambda (entry event)
	   (let* ((completion (who-completion tl-widget))
		  (completion-values (db-kise/select-distinct-who))
		  (completion-model (gtk2/create-completion-model completion-values)))
	     (set-model completion completion-model)
	     ;; (complete completion)
	     )
	   ;; gtk2 requirement ...
	   #f))

(connect (for-whom-entry tl-widget)
	 'focus-in-event
	 (lambda (entry event)
	   (let* ((completion (for-whom-completion tl-widget))
		  (completion-values (db-kise/select-distinct-for-whom))
		  (completion-model (gtk2/create-completion-model completion-values)))
	     (set-model completion completion-model))
	   ;; gtk2 requirement ...
	   #f))

(connect (what-combo-entry tl-widget)
	 'focus-in-event
	 (lambda (entry event)
	   (let* ((completion (what-completion tl-widget))
		  (completion-values `(,#("admin") ,#("asys") ,#("devel") ,#("maintenance")))
		  (completion-model (gtk2/create-completion-model completion-values)))
	     (set-model completion completion-model))
	   ;; gtk2 requirement ...
	   #f))

!#

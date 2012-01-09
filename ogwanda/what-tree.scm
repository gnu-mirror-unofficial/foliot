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

;; This an on going work. We think it would be ideal to have the what
;; combo displaying an activity tree, using a 'directory' open/colapse
;; mode as in file selection dialogs. This would work if users
;; effectively use an activity speparator, like we do, using #\/:
;; /sysadmin/install ...

;;; Code:

;; This code is not complete yet and will not be used in the first
;; releases.

(define-module (kise ogwanda what-tree)
  ;; guile/guile-gnome
  :use-module (ice-9 receive)
  :use-module (oop goops)
  :use-module (gnome gobject)
  :use-module (gnome gtk)

  ;; common
  :use-module (macros when)
  :use-module (macros do)
  :use-module (gtk all)

  ;; kise
  :use-module (kise tl-widget)

  :export (kwt/setup-what-treeview
	   kwt/fill-what-treeview
	   kwt/connect-what-tree))

(define (kwt/add-model treeview)
  (let ((model (gtk-tree-store-new (list <gchararray>))))
    (set-model treeview model)
    (values model
	    (get-selection treeview))))

(define (kwt/add-columns tl-widget treeview)
  (let* (;; WHAT
	 (renderer1 (make <gtk-cell-renderer-text>))
	 (column1   (make <gtk-tree-view-column>
		      :title       "What tree" ;; (_ "Code")
		      :clickable   #f
		      :resizable   #f
		      :reorderable #f
		      :alignment   .5))
	 (to-pack   `((what ,column1 ,renderer1 "text"))))
    (gtk2/pack-tv-cols treeview to-pack)))

(define (kwt/setup-what-treeview tl-widget)
  (let ((treeview (what-tv tl-widget)))
    (receive (model selection)
	(kwt/add-model treeview)
      (set-mode selection 'single)
      (set! (what-tv-model tl-widget) model)
      (set! (what-tv-sel tl-widget) selection))
    (kwt/add-columns tl-widget treeview)
    tl-widget))

(define (kwt/tree-add tl-widget what)
  (let* ((model (what-tv-model tl-widget))
	 (path-items (cdr (string-split what #\/)))
	 (its-length (length path-items)))
    ;; so, we look for the path in the gtktree
    ;; or add requested nodes so that this path exists
    ;; at the end of the execution of this function
    #t))

(define (kwt/fill-what-treeview tl-widget activities)
  (let ((model (what-tv-model tl-widget)))
    (gtk-tree-store-clear model)
    (for-each (lambda (what)
		;; what is a record from the db, containing 
		;; an what string as its first ref
		(kwt/tree-add tl-widget (vector-ref what 0)))
	activities)))

(define (kwt/fill-what-treeview tl-widget activities)
  (set! activities
	'(("admin" . ("email" "invoice"))
	  ("asys" . ("install" "upgrade"))))
  (let ((model (what-tv-model tl-widget)))
    (gtk-tree-store-clear model)
    (for-each (lambda (what)
		(let ((a-iter (gtk-tree-store-append model #f)))
		  (set-value model a-iter 0 (car what))
		  (for-each (lambda (subtype)
			      (let ((s-iter (gtk-tree-store-append model a-iter)))
				(set-value model s-iter 0 subtype)))
		      (cdr what))))
	activities)))


(define (kwt/connect-what-tree tl-widget)
  (connect (what-tv-model tl-widget)
	   'row-has-child-toggled
	   (lambda (path iter)
	     (format #t "row-has-child-toggled: ~S, ~S~%" path iter)
	     )))


#!

(use-modules (kise ogwanda what-tree))
(reload-module (resolve-module '(kise ogwanda what-tree)))


;;;
;;; Testing code
;;;


!#

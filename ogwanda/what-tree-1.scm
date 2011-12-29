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

;; We have abandonned this approach: it just consumes far too much gtk
;; energy [track all methods specifed here to be convinced yourself if
;; you wish to see how much.

;;; Code:

(define-module (kise ogwanda what-tree-1)
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

  :export (<what-tree>
	   kwt/setup-what-treeview))

(define-class <what-tree> (<guile-gtk-tree-model>)
  depth
  siblings)

(define-method (on-get-n-columns (obj <what-tree>))
  1)

(define-method (on-get-column-type (obj <what-tree>) index)
  <gchararray>)

(define-method (on-get-iter (obj <what-tree>) path)
  path)

(define-method (on-get-path (obj <what-tree>) iter)
  (format #t "on-iter-path: ~S~%" iter)
  iter)

(define-method (on-get-value (obj <what-tree>) iter index)
  (format #t "on-iter-value: Iter: ~S, Index: ~S~%" iter index)
  (format #f "~A" iter))

(define-method (on-iter-next (obj <what-tree>) iter)
  (format #t "on-iter-next: ~S~%" iter)
  (let* ((reversed (reverse iter))
         (next (1+ (car reversed))))
    (if (eq? next (slot-ref obj 'siblings))
        #f
        (reverse (cons next (cdr reversed))))))
    
(define-method (on-iter-children (obj <what-tree>) parent)
 (format #t "on-iter-children: ~S~%" parent)
  (cond
   ((not parent)
    (list 0))
   ((eq? (length parent) (slot-ref obj 'depth))
    #f)
   (else
    (reverse (cons 0 (reverse parent))))))

(define-method (on-iter-has-child (obj <what-tree>) iter)
  (format #t "on-iter-has-child: ~S~%" iter)
  (not (eq? (length iter) (slot-ref obj 'depth))))

(define-method (on-iter-n-children (obj <what-tree>) iter)
  (format #t "on-iter-n-children: ~S~%" iter)
  (cond
   ((not iter)
    (slot-ref obj 'siblings))
   ((on-iter-has-child obj iter)
    (slot-ref obj 'siblings))
   (else 0)))

(define-method (on-iter-nth-child (obj <what-tree>) parent n)
  (format #t "on-iter-nth-child: ~S ~S~%" parent n)
  (let ((nchildren (on-iter-n-children obj parent)))
    (if (< n nchildren)
        (reverse (cons n (if parent (reverse parent) '())))
        #f)))

(define-method (on-iter-parent (obj <what-tree>) iter)
   (format #t "on-iter-parent: ~S ~S~%" iter)
  (if (zero? (length iter))
      #f
      (reverse (cdr (reverse iter)))))

(define-method (initialize (obj <what-tree>) initargs)
  (next-method)
  (slot-set! obj 'depth 4)
  (slot-set! obj 'siblings 5))

(define (kwt/add-model treeview)
  (let ((model (make <what-tree>)))
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
		      :alignment   .5
		      ))
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


#!

(use-modules (kise ogwanda what-tree-1))
(reload-module (resolve-module '(kise ogwanda what-tree-1)))


;;;
;;; Testing code
;;;

(define *whats*
  '("/admin/email/tp5e-3"
    "/admin/tp5e-3"))


!#

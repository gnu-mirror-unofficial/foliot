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

(define-module (kise iter)
  ;; guile/guile-gnome
  :use-module (oop goops)
  :use-module (gnome gtk)
  :export (kiseiter/get
	   kiseiter/set
	   kiseiter/append-fill
	   kiseiter/prepend-fill))

(define kiseiter/get-pos #f)
(define kiseiter/get #f)
(define kiseiter/set #f)

(eval-when (compile load eval)
  (let ((offsets '((date . 0)
		   (date_ . 0)
		   (who . 1)
		   (for-whom . 2)
		   (for_whom . 2)
		   (duration . 3)
		   (to-be-charged . 4)
		   (to_be_charged . 4)
		   (what . 5))))
    (set! kiseiter/get-pos
	  (lambda (what) (cdr (assoc what offsets))))
    (set! kiseiter/get
	  (lambda (what model iter)
	    (get-value model iter (kiseiter/get-pos what))))
    (set! kiseiter/set
	  (lambda (what model iter value)
	    ;; (format #t "offset: ~S~%" (cdr (assoc what offsets)))
	    (set-value model iter (kiseiter/get-pos what) value)))
    ))

(define (kiseiter/append-fill model date who for-whom duration to-be-charged what)
  (let ((iter (gtk-list-store-append model)))
    (kiseiter/set 'date model iter date)
    (kiseiter/set 'who model iter who)
    (kiseiter/set 'for-whom model iter for-whom)
    (kiseiter/set 'duration model iter duration)
    (kiseiter/set 'to-be-charged model iter to-be-charged)
    (kiseiter/set 'what model iter what)
    iter))

(define (kiseiter/prepend-fill model date who for-whom duration to-be-charged what)
  (let ((iter (gtk-list-store-prepend model)))
    (kiseiter/set 'date model iter date)
    (kiseiter/set 'who model iter who)
    (kiseiter/set 'for-whom model iter for-whom)
    (kiseiter/set 'duration model iter duration)
    (kiseiter/set 'to-be-charged model iter to-be-charged)
    (kiseiter/set 'what model iter what)
    iter))


#!

(use-modules (kise iter))
(reload-module (resolve-module '(kise iter)))

!#
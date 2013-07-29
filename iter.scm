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

(define-module (kise iter)
  :use-module (oop goops)
  :use-module (gnome gtk)
  :export (kiter/get
	   kiter/set
	   kiter/append-fill
	   kiter/prepend-fill))

(define kiter/get-pos #f)
(define kiter/get #f)
(define kiter/set #f)

(eval-when (compile load eval)
  (let ((offsets '((icolour . 0)
		   (date . 1)
		   (date_ . 1)
		   (who . 2)
		   (for-whom . 3)
		   (for_whom . 3)
		   (duration . 4)
		   (to-be-charged . 5)
		   (to_be_charged . 5)
		   (what . 6)
		   (rowbg . 7)
		   (rowgg . 8)
		   (ibg . 9)
		   (ibg . 10))))
    (set! kiter/get-pos
	  (lambda (what) (cdr (assoc what offsets))))
    (set! kiter/get
	  (lambda (what model iter)
	    (get-value model iter (kiter/get-pos what))))
    (set! kiter/set
	  (lambda (what model iter value)
	    (set-value model iter (kiter/get-pos what) value)))))

(define (kiter/fill-next model iter date who for-whom duration to-be-charged what)
  (kiter/set 'date model iter date)
  (kiter/set 'who model iter who)
  (kiter/set 'for-whom model iter for-whom)
  (kiter/set 'duration model iter duration)
  (kiter/set 'to-be-charged model iter to-be-charged)
  (kiter/set 'what model iter what)
  iter)

(define (kiter/append-fill model date who for-whom duration to-be-charged what)
  (let ((iter (gtk-list-store-append model)))
    (kiter/fill-next model iter date who for-whom duration to-be-charged what)))

(define (kiter/prepend-fill model date who for-whom duration to-be-charged what)
  (let ((iter (gtk-list-store-prepend model)))
    (kiter/fill-next model iter date who for-whom duration to-be-charged what)))


#!

(use-modules (kise iter))
(reload-module (resolve-module '(kise iter)))

!#
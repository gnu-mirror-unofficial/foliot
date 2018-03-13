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


(define-module (foliot iter)
  #:use-module (oop goops)
  #:use-module (gnome gtk)

  #:duplicates (merge-generics
		replace
		warn-override-core
		warn
		last)

  #:export (fiter/get-pos
	    fiter/get
	    fiter/set
	    fiter/append-fill
	    fiter/prepend-fill))


(define fiter/get-pos #f)

(let ((foliot-iter-offsets '((icolour . 0)
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
			     (rowfg . 8)
			     (ibg . 9)
			     (ifg . 10))))
  (set! fiter/get-pos
	(lambda (key)
	  (assq-ref foliot-iter-offsets key))))

(define (fiter/get key model iter)
  (get-value model iter (fiter/get-pos key)))

(define (fiter/set key model iter value)
  (set-value model iter (fiter/get-pos key) value))

(define (fiter/fill model iter date who for-whom duration to-be-charged what ibg ifg)
  (fiter/set 'date model iter date)
  (fiter/set 'who model iter who)
  (fiter/set 'for-whom model iter for-whom)
  (fiter/set 'duration model iter (number->string duration))
  (fiter/set 'to-be-charged model iter to-be-charged)
  (fiter/set 'what model iter what)
  (fiter/set 'rowbg model iter #f)
  (fiter/set 'rowfg model iter #f)
  (fiter/set 'ibg model iter ibg)
  (fiter/set 'ifg model iter ifg)
  iter)

(define (fiter/append-fill model date who for-whom duration to-be-charged what ibg ifg)
  (let ((iter (gtk-list-store-append model)))
    (fiter/fill model iter date who for-whom duration to-be-charged what ibg ifg)))

(define (fiter/prepend-fill model date who for-whom duration to-be-charged what ibg ifg)
  (let ((iter (gtk-list-store-prepend model)))
    (fiter/fill model iter date who for-whom duration to-be-charged what ibg ifg)))

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


(define-module (kise db)
  ;; from guile

  ;; common
  #:use-module (macros reexport)
  ;#:use-module (system aglobs)
  ;#:use-module (system dates)
  ;#:use-module (db sqlite)

  ;; kise
  #:use-module (kise db-con)
  #:use-module (kise db-kise)
  #:use-module (kise db-imported-db)
  #:use-module (kise db-printing-templates)
  #:use-module (kise db-shinning)

  #:export (db/add-schema
	    db/check-schema
	    db/complete-schema))


(eval-when (compile load eval)
  (re-export-public-interface (kise db-con)
			      (kise db-kise)
			      (kise db-imported-db)
			      (kise db-printing-templates)
			      (kise db-shinning)))


;;;
;;; Schema
;;;

(define (db/add-schema db)
  (db-kise/add-kise-table db)
  (db-pt/add-printing-templates-table db)
  (db-idb/add-imported-db-table db)
  (db-shi/add-shinning-table db))

(define (db/check-schema db)
  (let ((kise? (sqlite/table-exists? db "kise"))
	(kise-printing-templates? (sqlite/table-exists? db "kise_printing_templates"))
	(kise-imported-db? (db-idb/check-schema db))
	(kise-shinning? (db-shi/check-schema db)))
    (cond ((and kise? ;; not good yet we should still check the defs
		kise-printing-templates? ;; not good yet we should still check the defs
		(eq? kise-imported-db? 'complete)
		(eq? kise-shinning? 'complete))
	   'complete)
	  ((or kise?
	       kise-printing-templates?
	       (eq? kise-imported-db? 'partial)
	       (eq? kise-shinning? 'partial))
	   'partial)
	  (else
	   'none))))

(define (db/complete-schema db)
  (db-kise/create-complete-table db)
  (db-pt/create-complete-table db)
  (db-idb/create-complete-table db)
  (db-shi/create-complete-table db))


#!

(use-modules (kise db))
(reload-module (resolve-module '(kise db)))

!#

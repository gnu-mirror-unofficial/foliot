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

(define-module (kise db)
  ;; from guile

  ;; common
  :use-module (macros reexport)
  :use-module (macros when)
  ;:use-module (system aglobs)
  ;:use-module (system dates)
  ;:use-module (db sqlite)

  ;; kise
  :use-module (kise db-con)
  :use-module (kise db-kise)
  :use-module (kise db-printing-templates)

  :export (db/add-schema
	   db/check-schema
	   db/complete-schema))


(eval-when (compile load eval)
  (re-export-public-interface (kise db-con)
			      (kise db-kise)
			      (kise db-printing-templates)))


;;;
;;; Schema
;;;

(define (db/add-schema)
  (db-kise/add-kise-table)
  (db-pt/add-printing-templates-table))

(define (db/check-schema)
  (let* ((db (db-con))
	 (kise? (sqlite/table-exists? db "kise"))
	 (kise-printing-templates? (sqlite/table-exists? db "kise_printing_templates")))
    (cond ((and kise? kise-printing-templates?) 'complete)
	  ((or kise? kise-printing-templates?) 'partial)
	  (else
	   'none))))

(define (db/complete-schema)
  (let ((db (db-con)))
    (unless (sqlite/table-exists? db "kise") (db-kise/add-kise-table))
    (unless (sqlite/table-exists? db "kise_printing_templates") (db-pt/add-printing-templates-table))))


#!

(use-modules (kise db))
(reload-module (resolve-module '(kise db)))

!#
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

(define-module (kise db-con)
  ;; common
  :use-module (macros reexport)
  :use-module (system aglobs)
  :use-module (db sqlite)


  :export (db-con
	   db-con/open
	   db-con/close
	   
	   db-con/add-schema
	   ))


(eval-when (compile load eval)
  (re-export-public-interface (system aglobs)
			      (db sqlite)))


;;;
;;; Db Connector
;;;

(define (db-con)
  (aglobs/get 'db-con))

(define (db-con/open filename)
  (let ((db (sqlite-open filename 6)))
    (and db
	 (aglobs/set 'db-con db)
	 db)))

(define (db-con/close)
  (sqlite-close (db-con))
  (aglobs/set 'db-con #f))


#!

(use-modules (kise db-con))
(reload-module (resolve-module '(kise db-con)))

(db-con/open "/tmp/new.db")
(db-con/close)

!#

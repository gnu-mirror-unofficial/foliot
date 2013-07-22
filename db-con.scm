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

(define-module (kise db-con)
  ;; common
  :use-module (macros reexport)
  :use-module (system aglobs)
  :use-module (db sqlite)

  :export (db-con
	   db-name
	   db-con/open
	   db-con/close
	   
	   db-con/add-schema))


(eval-when (compile load eval)
  (re-export-public-interface (system aglobs)
			      (db sqlite)))


;;;
;;; Db Connector
;;;

(define (db-con)
  (aglobs/get 'db-con))

(define (db-name)
  (aglobs/get 'db-name))

(define (db-con/load-pcre-ext-str)
  "select load_extension('~A')")

(define* (db-con/open filename #:optional (set-db-con? #t))
  (let ((db-name (basename filename))
	(db (sqlite-open filename)) ;; 6
	(pcre-lib-ext "/usr/lib/sqlite3/pcre.so"))
    (if db
	(begin
	  (when set-db-con?
	    (aglobs/set 'db-con db)
	    (aglobs/set 'db-name db-name)
	    (when (access? pcre-lib-ext R_OK)
	      (sqlite-enable-load-extension db 1)
	      (sqlite/query db (format #f "~?" (db-con/load-pcre-ext-str) (list pcre-lib-ext)))
	      (sqlite-enable-load-extension db 0) ;; avoiding security holes
	      (aglobs/set 'db-pcre #t)))
	  db)
	#f)))

(define* (db-con/close db #:optional (set-db-con? #t))
  (sqlite-close db)
  (when set-db-con?
    (aglobs/set 'db-con #f)
    (aglobs/set 'db-name #f)))


#!

(use-modules (kise db-con))
(reload-module (resolve-module '(kise db-con)))

(db-con/open "/tmp/new.db")
(db-con/close)

!#

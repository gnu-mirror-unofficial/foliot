;; -*- mode: scheme; coding: utf-8 -*-

;;;;
;;;; Copyright (C) 2011 - 2016
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


(define-module (foliot db-con)
  #:use-module (oop goops)
  #:use-module (grip module)
  #:use-module (grip utils)
  #:use-module (grip sqlite)
  #:use-module (foliot globals)

  #:export (db-con
	    db-name
	    db-con/set-db-con
	    db-con/open
	    db-con/close))


(eval-when (expand load eval)
  (re-export-public-interface (grip utils)
			      (grip sqlite)))


;;;
;;; Db Connector
;;;

(define (db-con)
  (ref %foliot-store 'db-con))

(define (db-name)
  (ref %foliot-store 'db-name))

(define (db-con/set-db-con db db-name)
  (let ((pcre-lib-ext "/usr/lib/sqlite3/pcre.so"))
    (set-! %foliot-store 'db-con db)
    (set-! %foliot-store 'db-name db-name)
    (when (access? pcre-lib-ext R_OK)
      (sqlite-enable-load-extension db 1)
      (sqlite/query db (format #f "~?" (db-con/load-pcre-ext-str) (list pcre-lib-ext)))
      (sqlite-enable-load-extension db 0) ;; avoiding security holes
      (set-! %foliot-store 'db-pcre #t))))

(define (db-con/load-pcre-ext-str)
  "select load_extension('~A')")

(define* (db-con/open filename #:optional (set-db-con? #t))
  (let ((db-name (basename filename))
	(db (sqlite-open filename))) ;; 6
    (if db
	(begin
	  (when set-db-con? (db-con/set-db-con db db-name))
	  db)
	#f)))

(define* (db-con/close db #:optional (set-db-con? #t))
  (sqlite-close db)
  (when set-db-con?
    (set-! %foliot-store 'db-con #f)
    (set-! %foliot-store 'db-name #f)))


#!

(db-con/open "/tmp/new.db")
(db-con/close)

!#

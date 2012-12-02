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

;;; Code:

(define-module (kise db-imported-db)
  ;; guile
  :use-module (ice-9 format)

  ;; common
  :use-module (macros reexport)
  :use-module (macros do)
  :use-module (db sqlite)
  :use-module (system dates)
  :use-module (system i18n)
  :use-module (system aglobs)
  :use-module (strings strings)

  ;; kise
  :use-module (kise globals)
  :use-module (kise db-con)

  :export (db-idb/add-imported-db-table
	   db-idb/create-complete-table
	   db-idb/select-all
	   db-idb/select-some
	   db-idb/get
	   db-idb/set
	   db-idb/get-tuple
	   db-idb/update
	   db-idb/find-pos
	   db-idb/get-next-id
	   db-idb/add
	   db-idb/duplicate
	   db-idb/delete))


(eval-when (compile load eval)
  (re-export-public-interface (db sqlite)
			      (system dates)
			      (system i18n)
			      (system aglobs)
			      (strings strings)
			      (kise globals)
			      (kise db-con))
  (textdomain "db-imported-db")
  (bindtextdomain "db-imported-db" (aglobs/get 'pofdir)))


;;;
;;; Create table
;;;

(define (db-idb/add-imported-db-table-str)
  "create table kise_imported_db (
     id               integer primary key not null,
     name             text,
     imported_the     integer,
     imported_by      text
);")

(define (db-idb/add-imported-db-table)
  (sqlite/command (db-con) 
		  (db-idb/add-imported-db-table-str)))

(define (db-idb/create-complete-table)
  (let* ((db (db-con))
	 (exists? (sqlite/table-exists? db "kise_imported_db")))
   (unless exists? (db-idb/add-imported-db-table))))


;;;
;;; Select
;;;

(define (db-idb/select-one-str)
  "select *
     from kise_imported_db
    where id = '~A';")

(define (db-idb/select-one reference)
  (sqlite/query (db-con)
		(format #f "~?" (db-idb/select-one-str)
			(list reference))))

(define (db-idb/select-all-str)
  "select *
     from kise_imported_db
 order by name;")

(define (db-idb/select-all)
  (sqlite/query (db-con) (db-idb/select-all-str)))

(define (db-idb/select-some-str)
  "select *
     from kise_imported_db
    where ~A
 order by name;")

(define (db-idb/select-some where)
  (sqlite/query (db-con)
		(format #f "~?" (db-idb/select-some-str)
			(list where))))

;;;
;;; Attr pos
;;;

(define (db-idb/fields-offsets)
  '((id . 0)
    (name . 1)
    (imported_the . 2)
    (imported_by . 3)))

(define (db-idb/get-pos what)
  (cdr (assoc what (db-idb/fields-offsets))))


;;;
;;; Later a global API
;;;

(define (db-idb/get db-tuple what)
  ;; db-tuple is a vector
  (vector-ref db-tuple (db-idb/get-pos what)))

(define (db-idb/set db-tuple what value)
  ;; db-tuple is a vector
  (vector-set! db-tuple (db-idb/get-pos what) value))

(define (db-idb/get-tuple tuples offset)
  ;; so far, tuples is a list
  (list-ref tuples offset))


;;;
;;; Updates
;;;

(define (db-idb/set-str)
  "update kise_imported_db
      set ~A = '~A'
    where id = '~A';")

(define (db-idb/update db-tuple what value . displayed-value)
  (let* ((id (db-idb/get db-tuple 'id))
	 (sql-value (case what
		      ((name) (str/prep-str-for-sql value))
		      (else
		       value)))
	 (cmd (format #f "~?" (db-idb/set-str) (list what sql-value id))))
    ;; (format #t "~S~%Displayed value: ~S~%" cmd displayed-value)
    (sqlite/command (db-con) cmd)
    (if (null? displayed-value)
	(db-idb/set db-tuple what value)
	(db-idb/set db-tuple what (car displayed-value)))
    ;; updated? reordered?
    (values #t #f)))

(define (db-idb/find-pos tuples what value pred)
  (let ((its-length (length tuples))
	(accessor (if (symbol? what) db-idb/get vector-ref)))
    (and (> its-length 0)
	 (catch 'exit
	   (lambda ()
	     (do ((i 0 (1+ i)))
		 ((= i its-length) #f)
	       (let ((db-tuple (list-ref tuples i)))
		 (if (pred (accessor db-tuple what) value)
		     (throw 'exit i)))))
	   (lambda (key index)
	     index)))))


;;;
;;; Add / Dupplcate
;;;

(define (db-idb/get-next-id-str)
  "select max(id) from kise_imported_db;")

(define (db-idb/get-next-id)
  (let* ((next (sqlite/query (db-con)
			     (db-idb/get-next-id-str)))
	 (value (vector-ref (car next) 0)))
    ;; (format #t "db-idb/get-next-id: ~S. value: ~S~%" next value)
    (if value (1+ value) 0)))

(define (db-idb/add-str)
  "insert into kise_imported_db (id, name, imported_the, imported_by)
   values ('~A', '~A', strftime('%s','~A'), '~A');")

(define (db-idb/add name date who)
  (let* ((next-id (db-idb/get-next-id))
	 (insert (format #f "~?" (db-idb/add-str)
			 (list next-id name date who))))
    ;; (format #t "~S~%" insert)
    (sqlite/command (db-con) insert)
    next-id))


;;;
;;; Delete
;;;

(define (db-idb/delete-str)
  "delete from kise_imported_db
    where id = '~A';")

(define (db-idb/delete reference)
  (sqlite/command (db-con)
		  (format #f "~?" (db-idb/delete-str)
			  (list reference))))


#!

(use-modules (kise db-imported-db))
(reload-module (resolve-module '(kise db-imported-db)))

(db-con/open "/usr/alto/db/sqlite.alto.db")
(db-idb/select-all)

(db-idb/add "sqlite.alto.christian.db" "2011-06-14" "david")

(db-idb/select-some "name = 'sqlite.alto.christian.db'")

!#

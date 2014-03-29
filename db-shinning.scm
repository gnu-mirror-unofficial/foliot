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

(define-module (kise db-shinning)
  ;; guile
  :use-module (ice-9 format)
  :use-module (ice-9 threads)
  :use-module (srfi srfi-1)

  ;; common
  :use-module (macros reexport)
  :use-module (macros do)
  :use-module (db sqlite)
  :use-module (system i18n)
  :use-module (system aglobs)
  :use-module (nbs all)
  :use-module (strings strings)

  ;; kise
  :use-module (kise globals)
  :use-module (kise db-con)
  :use-module (kise db-kise)

  :export (db-shi/add-shinning-table
	   db-shi/check-schema
	   db-shi/create-complete-table
	   db-shi/select-all
	   db-shi/select-one
	   db-shi/get
	   db-shi/set
	   db-shi/get-tuple
	   db-shi/update
	   db-shi/get-next-id
	   db-shi/add
	   db-shi/delete
	   run-shinning-room-237-checks))


(eval-when (compile load eval)
  (re-export-public-interface (macros do)
			      (db sqlite)
			      (system i18n)
			      (system aglobs)
			      (strings strings)
			      (kise globals)
			      (kise db-con))
  (textdomain "db-shinning")
  (bindtextdomain "db-shinning" (aglobs/get 'pofdir)))


;;;
;;; Globals
;;;

(define (db-shi/fields)
  "id, room_237")


;;;
;;; Attr pos, get, set
;;;

(define (db-shi/fields-offsets)
  '((id . 0)
    (room_237 . 1)))

(define (db-shi/get-pos what)
  (assq-ref (db-shi/fields-offsets) what))

(define (db-shi/get db-tuple what)
  ;; db-tuple is a vector
  (vector-ref db-tuple (db-shi/get-pos what)))

(define (db-shi/set db-tuple what value)
  ;; db-tuple is a vector
  (vector-set! db-tuple (db-shi/get-pos what) value))

(define (db-shi/get-tuple tuples offset)
  ;; so far, tuples is a list
  (list-ref tuples offset))


;;;
;;; Create table
;;;

(define (db-shi/add-shinning-table-str)
  "create table kise_shinning (
     id               integer primary key not null,
     room_237         text
);")

(define (db-shi/add-shinning-table db)
  (sqlite/command db (db-shi/add-shinning-table-str)))

(define (db-shi/check-schema db)
  ;; 'none, 'partial, 'complete
  (if (sqlite/table-exists? db "kise_shinning")
      (let* ((table-info (sqlite/table-info db "kise_shinning"))
	     (cols-nb (length table-info)))
	(cond ((= cols-nb 1) ;; impossible, just holding the code for 'partial case
	       'partial)     ;; for a possible future need...
	      ((= cols-nb 2)
	       'complete)))
      'none))

(define (db-shi/create-complete-table db)
  (if (sqlite/table-exists? db "kise_shinning")
      #t
      (db-shi/add-shinning-table db)))


;;;
;;; Select
;;;

(define (db-shi/select-one-str)
  "select *
     from kise_shinning
    where id = '~A';")

(define (db-shi/select-one reference)
  ;; guile-sqlite [always] returns a list
  (let ((result (sqlite/query (db-con)
			      (format #f "~?" (db-shi/select-one-str)
				      (list reference)))))
    (and (not (null? result)) (car result))))

(define (db-shi/select-all-str)
  "select *
     from kise_shinning;")

(define (db-shi/select-all)
  (sqlite/query (db-con) (db-shi/select-all-str)))


;;;
;;; Updates
;;;

(define *update-str*
  "update kise_shinning
      set ~A = '~A'
    where id = '~A';")

(define (db-shi/update db-tuple what value)
  (let* ((id (db-shi/get db-tuple 'id))
	 (cmd (format #f "~?" *update-str* (list what value id))))
    (sqlite/command (db-con) cmd)
    (db-shi/set db-tuple what value)
    ;; updated? reordered?
    (values #t #f)))


;;;
;;; Add
;;;

(define (db-shi/get-next-id-str)
  "select max(id) from kise_shinning;")

(define (db-shi/get-next-id)
  (let* ((next (sqlite/query (db-con)
			     (db-shi/get-next-id-str)))
	 (value (vector-ref (car next) 0)))
    (if value (1+ value) 0)))

(define (db-shi/add-str)
  "insert into kise_shinning (id, room_237)
          values ('~A','~A');")

(define* (db-shi/add room-237 #:optional (id #f))
  (let* ((next-id (or id (db-shi/get-next-id)))
	 (insert (format #f "~?" (db-shi/add-str)
			 (list next-id room-237))))
    ;; (dimfi insert)
    (sqlite/command (db-con) insert)
    next-id))


;;;
;;; Delete
;;;

(define (db-shi/delete-str)
  "delete from kise_shinning
    where id = '~A';")

(define (db-shi/delete reference)
  (sqlite/command (db-con)
		  (format #f "~?" (db-shi/delete-str)
			  (list reference))))


;;;
;;; Shinning room 237 checks
;;;

(define (db-shi/update-flag flag flags tuple db)
  (if tuple
      (if flags
	  (db-shi/update tuple 'room_237 (cons '(floats-1dec-only . #t) flags))
	  (db-shi/update tuple 'room_237 (list (cons 'floats-1dec-only #t))))
      (db-shi/add (list (cons 'floats-1dec-only #t)) 0)))

(define (room-237-check-floats-1dec-only flags tuple db)
  (unless (assq-ref flags 'floats-1dec-only)
    (sqlite/begin-transaction db)
    (let ((tuples (db-kise/select-all)))
      (par-map (lambda (tuple)
		 (db-kise/update tuple 'duration
				 (fp/round (db-kise/get tuple 'duration) 1)))
	  tuples))
    (sqlite/commit db)
    (db-shi/update-flag 'floats-1dec-only flags tuple db)))

(define (run-shinning-room-237-checks db)
  ;; 1- until we knew, not all floats where stored in the db with 1 decimal only.
  ;; this is because of a bug, in my opinion, in the gtk-spin-button widget.  we
  ;; modified the duration spin-button callback, but existing databases must be
  ;; parsed, once only, to reset the duration floats to 1 decimal only.
  (let* ((tuple (db-shi/select-one 0))
	 (flags (and tuple (with-input-from-string (db-shi/get tuple 'room_237) read))))
    (room-237-check-floats-1dec-only flags tuple db)))


#!

(use-modules (kise db-shinning))
(reload-module (resolve-module '(kise db-shinning)))

(db-con/open "/usr/alto/db/sqlite.alto.tests.db")
(db-con/open "/usr/alto/db/bryony.db")
(db-shi/select-all)

!#

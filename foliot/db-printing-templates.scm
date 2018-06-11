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


(define-module (foliot db-printing-templates)
  #:use-module (ice-9 format)
  ;; #:use-module (ice-9 receive)
  ;; #:use-module (oop goops)
  #:use-module (grip module)
  #:use-module (grip iter)
  #:use-module (grip sqlite)
  #:use-module (grip date)
  #:use-module (grip i18n)
  #:use-module (grip utils)
  #:use-module (grip string)
  #:use-module (foliot globals)
  #:use-module (foliot db-con)

  #:export (db-pt/add-printing-templates-table
	    db-pt/create-complete-table
	    db-pt/select-all
	    db-pt/select-some
	    db-pt/get
	    db-pt/set
	    db-pt/get-tuple
	    db-pt/update
	    db-pt/find-pos
	    db-pt/get-next-id
	    db-pt/add
	    db-pt/get-tex-field-spec
	    db-pt/treeview-field-specs
	    db-pt/df-get
	    db-pt/add-default
	    db-pt/duplicate
	    db-pt/delete))


(eval-when (expand load eval)
  (re-export-public-interface (grip sqlite)
			      (grip date)
			      (grip i18n)
			      (grip utils)
			      (grip string)
			      (foliot globals)
			      (foliot db-con))
  (textdomain "db-printing-templates")
  (bindtextdomain "db-printing-templates" (ref %foliot-store 'pofdir)))


;;;
;;; Create table
;;;

(define (db-pt/add-printing-templates-table-str)
  "create table foliot_printing_templates (
     id               integer primary key not null,
     name             text,
     items            text,
     mode             text,
     group_and_sort   text
);")

(define (db-pt/add-printing-templates-table db)
  (sqlite/command db (db-pt/add-printing-templates-table-str)))

(define (db-pt/create-complete-table db)
  (unless (sqlite/table-exists? db "foliot_printing_templates") (db-pt/add-printing-templates-table db)))


;;;
;;; Select
;;;

(define (db-pt/select-one-str)
  "select *
     from foliot_printing_templates
    where id='~A';")

(define (db-pt/select-one reference)
  (sqlite/query (db-con)
		(format #f "~?" (db-pt/select-one-str)
			(list reference))))

(define (db-pt/select-all-str)
  "select *
     from foliot_printing_templates
 order by name;")

(define (db-pt/select-all)
  (sqlite/query (db-con) (db-pt/select-all-str)))

(define (db-pt/select-some-str)
  "select *
     from foliot_printing_templates
    where ~A
 order by name;")

(define (db-pt/select-some where)
  (sqlite/query (db-con)
		(format #f "~?" (db-pt/select-some-str)
			(list where))))

;;;
;;; Attr pos
;;;

(define (db-pt/fields-offsets)
  '((id . 0)
    (name . 1)
    (items . 2)
    (mode . 3)
    (group_and_sort . 4)))

(define (db-pt/get-pos what)
  (cdr (assoc what (db-pt/fields-offsets))))


;;;
;;; Later a global API
;;;

(define (db-pt/get db-tuple what)
  ;; db-tuple is a vector
  (vector-ref db-tuple (db-pt/get-pos what)))

(define (db-pt/set db-tuple what value)
  ;; db-tuple is a vector
  (vector-set! db-tuple (db-pt/get-pos what) value))

(define (db-pt/get-tuple tuples offset)
  ;; so far, tuples is a list
  (list-ref tuples offset))


;;;
;;; Updates
;;;

(define (db-pt/set-str)
  "update foliot_printing_templates
      set ~A = '~A'
    where id = '~A';")

(define (db-pt/update db-tuple what value . displayed-value)
  (let* ((id (db-pt/get db-tuple 'id))
	 (sql-value (case what
		      ((name) (string-escape-sql value))
		      (else
		       value)))
	 (cmd (format #f "~?" (db-pt/set-str) (list what sql-value id))))
    ;; (format #t "~S~%Displayed value: ~S~%" cmd displayed-value)
    (sqlite/command (db-con) cmd)
    (if (null? displayed-value)
	(db-pt/set db-tuple what value)
	(db-pt/set db-tuple what (car displayed-value)))
    ;; updated? reordered?
    (values #t #f)))

(define (db-pt/find-pos tuples what value pred)
  (let ((its-length (length tuples))
	(accessor (if (symbol? what) db-pt/get vector-ref)))
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

(define (db-pt/get-next-id-str)
  "select max(id) from foliot_printing_templates;")

(define (db-pt/get-next-id)
  (let* ((next (sqlite/query (db-con)
			     (db-pt/get-next-id-str)))
	 (value (vector-ref (car next) 0)))
    ;; (format #t "db-pt/get-next-id: ~S. value: ~S~%" next value)
    (if value (1+ value) 0)))

(define (db-pt/add-str)
  "insert into foliot_printing_templates (id, name, items, mode, group_and_sort)
   values ('~A', '~A', '~A', '~A', '~A');")

(define (db-pt/add name items mode group-and-sort)
  (let* ((next-id (db-pt/get-next-id))
	 (insert (format #f "~?" (db-pt/add-str)
			 (list next-id (string-escape-sql name) items mode group-and-sort))))
    ;; (format #t "~S~%" insert)
    (sqlite/command (db-con) insert)
    next-id))

(define (db-pt/tex-field-specs)
  ;; spec: (name db-field-pos tex-ltx-col-spec needs-prep-
  '((for_whom . (for_whom 3 "l" #t))
    (date_ . (date_ 1 "c" #f))
    (what . (what 7 "l" #t))	;; new name
    (description . (description 8 "X" #t))
    (duration . (duration 4 "n{2}{1}" #f))
    (to_be_charged . (to_be_charged 5 "c" #f))
    (who . (who 2 "l" #t))
    (id . (id 0 "n{4}{0}" #f))))

(define (db-pt/get-tex-field-spec which)
  (cdr (assoc which (db-pt/tex-field-specs))))

(define (db-pt/treeview-field-specs)
  ;; spec: (print? group? display-name sort-mode)
  '((#t #t "for_whom" asc) 
    (#t #t "date_" desc)
    (#t #f "what" asc)
    (#t #f "description" none)
    (#t #f "duration" none)
    (#t #f "to_be_charged" none)
    (#f #f "who" asc)
    (#f #f "id" none)))

(define (db-pt/df-get what field)
  (list-ref field 
	    (case what
	      ((print) 0)
	      ((group) 1)
	      ((name) 2)
	      ((sort) 3))))

(define (db-pt/add-default . name)
  (db-pt/add (if (null? name) (_ "default") (car name))
	     "all"
	     "draft"
	     (format #f "~S" (db-pt/treeview-field-specs))))

(define (db-pt/duplicate reference . tuple)
  (if (null? tuple)
      (set! tuple (car (db-pt/select-one reference)))
      (set! tuple (car tuple)))
  (db-pt/add (string-append (string-escape-sql (db-pt/get tuple 'name)) "1")
	     (db-pt/get tuple 'items)
	     (db-pt/get tuple 'mode)
	     (db-pt/get tuple 'group_and_sort)))


;;;
;;; Delete
;;;

(define (db-pt/delete-str)
  "delete from foliot_printing_templates
    where id = '~A';")

(define (db-pt/delete reference)
  (sqlite/command (db-con)
		  (format #f "~?" (db-pt/delete-str)
			  (list reference))))


#!

(db-foliot/open "/usr/alto/db/sqlite.alto.db")
(db-pt/select-all)

(define gr 
  (with-input-from-string "((#t #t \"for_whom\" asc) (#t #f \"date_\" desc))" read))

(define gr1 (car gr))
(if (car gr1) "yes" "no")
(case (cadddr gr1) ((asc) "asc") (else "other"))

!#

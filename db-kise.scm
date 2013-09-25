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

(define-module (kise db-kise)
  ;; guile
  :use-module (ice-9 format)
  :use-module (ice-9 receive)
  :use-module (oop goops)

  ;; common
  :use-module (macros reexport)
  :use-module (macros do)
  :use-module (system dates)
  :use-module (system i18n)
  :use-module (system aglobs)
  :use-module (system passwd)
  :use-module (strings strings)
  :use-module (db sqlite)
  :use-module (gtk colours)

  ;; kise
  :use-module (kise globals)
  :use-module (kise db-con)
  :use-module (kise db-imported-db)
  
  :export (db-kise/add-kise-table
	   db-kise/complete-table
	   db-kise/create-complete-table
	   db-kise/prepend-empty
	   db-kise/select-one
	   db-kise/select-all
	   db-kise/select-all-other-db
	   db-kise/select-some
	   db-kise/select-another-some
	   db-kise/select-distinct
	   db-kise/get-pos
	   db-kise/get
	   db-kise/set
	   db-kise/get-tuple
	   db-kise/update
	   db-kise/find-pos
	   db-kise/get-next-id
	   db-kise/add
	   db-kise/add-from-other-db
	   db-kise/duplicate
	   db-kise/delete
	   db-kise/delete-some
	   db-kise/import
	   db-kise/delete-imported-tuples))


(eval-when (compile load eval)
  (re-export-public-interface (db sqlite)
			      (system dates)
			      (system i18n)
			      (system aglobs)
			      (system passwd)
			      (strings strings)
			      (kise globals)
			      (kise db-con)
			      (kise db-imported-db))
  (textdomain "db-kise")
  (bindtextdomain "db-kise" (aglobs/get 'pofdir)))


;;;
;;; Globals
;;;

(define (db-kise/fields)
  (let ((sep "."))
    (format #f "id,
   strftime('%d~A%m~A%Y', date_, 'unixepoch'),
   who,
   for_whom,
   duration,
   to_be_charged,
   charging_type,
   what,
   description,
   strftime('%d~A%m~A%Y', created_the, 'unixepoch'),
   created_by,
   strftime('%d~A%m~A%Y', modified_the, 'unixepoch'),
   modified_by,
   imported_id,
   imported_db"
	    sep sep sep sep sep sep)))

;;;
;;; Attr pos, get, set
;;;

(define (db-kise/fields-offsets)
  '((id . 0)
    (date_ . 1)
    (who . 2)
    (for_whom . 3)
    (duration . 4)
    (to_be_charged . 5)
    (charging_type . 6)
    (what . 7)
    (description . 8)
    (created_the . 9)
    (created_by . 10)
    (modified_the . 11)
    (modified_by . 12)
    (imported_id . 13)
    (imported_db . 14)))

(define (db-kise/get-pos what)
  (assq-ref (db-kise/fields-offsets) what))

(define (db-kise/get db-tuple what)
  ;; db-tuple is a vector. NULL values are returned as #f by sqlite.scm
  ;; which is a problem if the field is used in _text_ widget
  (let ((value (vector-ref db-tuple (db-kise/get-pos what))))
    (case what
      ((date date_) (if value value ""))
      (else value))))

(define (db-kise/set db-tuple what value)
  ;; db-tuple is a vector
  (vector-set! db-tuple (db-kise/get-pos what) value))

(define (db-kise/get-tuple tuples offset)
  ;; so far, tuples is a list
  (list-ref tuples offset))


;;;
;;; Non api stuff
;;;

(define (db-kise/prepend-empty tuples . v-size)
  ;; tuples is a list AND, if there is an empty tuple, this function
  ;; assumes its pos is 0, otherwise it prepends tuples with an empty one
  (cond ((null? tuples)
	 (if (null? v-size)
	     (list #(""))
	     (list (make-vector (car v-size) ""))))
	(else
	 (let* ((tuple (list-ref tuples 0))
		(its-length (vector-length tuple))
		(empty-v (make-vector its-length "")))
	   (if (equal? tuple empty-v)
	       tuples
	       (cons empty-v tuples))))))

;;;
;;; Schema related
;;;

(define (db-kise/add-kise-table-str)
  "create table kise (
     id             integer primary key not null,
     date_          integer,
     who            text,
     for_whom       text,
     duration       float,
     to_be_charged  text,
     charging_type  text,
     what           text,
     description    text,
     created_the    integer,
     created_by     text,
     modified_the   integer,
     modified_by    text,
     imported_id    integer,
     imported_db    integer
   );")

(define (db-kise/add-kise-table)
  (sqlite/command (db-con) (db-kise/add-kise-table-str)))

(define (db-kise/complete-table)
  (let* ((db (db-con))
	 (table-info (sqlite/table-info db "kise")))
    (unless (sqlite/tuple-pos "imported_id" table-info string=? 1)
      ;; upgrading from 0.9.1 to 0.9.2
      (sqlite/add-column db "kise" "imported_id integer default '-1' not null")
      (sqlite/add-column db "kise" "imported_db integer default '-1' not null"))))

(define (db-kise/create-complete-table)
  (let* ((db (db-con))
	 (exists? (sqlite/table-exists? db "kise")))
   (if exists?
       (db-kise/complete-table)
       (db-kise/add-kise-table))))


;;;
;;; Select
;;;

(define (db-kise/select-one-str)
  "select ~A
     from kise
    where id='~A'")

(define (db-kise/select-one reference)
  (sqlite/query (db-con)
		(format #f "~?" (db-kise/select-one-str)
			(list (db-kise/fields)
			      reference))))

(define (db-kise/select-order-by-str)
  "date_ desc, who asc, for_whom asc, what asc, to_be_charged asc, id desc")

(define (db-kise/select-all-str)
  "select ~A
     from kise
 order by ~A")

(define (db-kise/select-all . aggregate?)
  (let ((what (if (null? aggregate?) (db-kise/fields) (car aggregate?))))
    (sqlite/query (db-con)
		  (format #f "~?" (db-kise/select-all-str)
			  (list what
				(db-kise/select-order-by-str))))))

(define (db-kise/select-all-other-db-str)
  ;; do not process dates, they will be imported as is
  "select *
     from kise
 order by ~A")

(define (db-kise/select-all-other-db db)
  (sqlite/query db
		(format #f "~?" (db-kise/select-all-other-db-str)
			(list (db-kise/select-order-by-str)))))

(define (db-kise/select-some-str)
  "select ~A
     from kise
    where ~A
 order by ~A")

(define (db-kise/select-some-with-ids-str)
  "select ~A
     from kise
    where ~A
       or id in ~A
 order by ~A")

(define (db-kise/select-some where ids . aggregate?)
  (let ((what (if (null? aggregate?) (db-kise/fields) (car aggregate?))))
    (cond ((and where ids)
	   (sqlite/query (db-con)
			 (format #f "~?" (db-kise/select-some-with-ids-str)
				 (list what
				       where
				       (sqlite/build-set-expression ids)
				       (db-kise/select-order-by-str)))))
	  (where
	   (sqlite/query (db-con)
			 (format #f "~?" (db-kise/select-some-str)
				 (list what
				       where
				       (db-kise/select-order-by-str)))))
	  (else
	   (if (null? aggregate?)
	       (db-kise/select-all)
	       (db-kise/select-all (car aggregate?)))))))

(define (db-kise/select-another-some-str mode)
  (case mode
    ((1) "select ~A from kise where ~A group by ~A order by ~A")
    ((2) "select ~A from kise where ~A group by ~A")
    ((3) "select ~A from kise where ~A")
    ((4) "select ~A from kise where ~A order by ~A")
    ((5) "select ~A from kise group by ~A order by ~A")
    ((6) "select ~A from kise group by ~A")
    ((7) "select ~A from kise order by ~A")
    ((8) "select ~A from kise")))

(define (db-kise/select-another-some where group-by order-by)
  (let* ((mode (cond ((and where group-by order-by) 1)
		     ((and where group-by) 2)
		     ((and where order-by) 4)
		     (where 3)
		     ((and group-by order-by) 5)
		     (group-by 6)
		     (order-by 7)
		     (else
		      8)))
	 (query-string (format #f "~?" (db-kise/select-another-some-str mode)
			       (case mode
				 ((1) (list (db-kise/fields) where group-by order-by))
				 ((2) (list (db-kise/fields) where group-by))
				 ((3) (list (db-kise/fields) where))
				 ((4) (list (db-kise/fields) where order-by))
				 ((5) (list (db-kise/fields) group-by order-by))
				 ((6) (list (db-kise/fields) group-by))
				 ((7) (list (db-kise/fields) order-by))
				 ((8) (list (db-kise/fields)))))))
    ;; (format #t "db-kise/select-another-some~%  ~S~%" query-string)
    (sqlite/query (db-con) query-string)))

(define (db-kise/select-distinct-str)
  "select distinct(~A) from kise order by ~A;")

(define (db-kise/select-distinct colname . add-empty?)
  ;; because the results of this function is also used to build combo,
  ;; we need the ability to add an empty entry id necessary.
  (let ((distincts (sqlite/query (db-con)
				 (format #f "~?" (db-kise/select-distinct-str)
					 (list colname colname)))))
    (if (null? add-empty?)
	distincts
	(db-kise/prepend-empty distincts))))


;;;
;;; Updates
;;;

(define (db-kise/set-date-str)
  "update kise
      set ~A = strftime('%s','~A')
    where id = '~A';")

(define (db-kise/set-str)
  "update kise
      set ~A = '~A'
    where id = '~A';")

(define (db-kise/update db-tuple what value . displayed-value)
  (let* ((id (db-kise/get db-tuple 'id))
	 (sql-value (case what
		      ((who for_whom what description) (str/prep-str-for-sql value))
		      (else
		       value)))
	 (sql-str (case what
		    ((date_ created_the modified_the) (db-kise/set-date-str))
		    (else
		     (db-kise/set-str))))
	 (cmd (format #f "~?" sql-str (list what sql-value id))))
    ;; (format #t "~S~%Displayed value: ~S~%" cmd displayed-value)
    (sqlite/command (db-con) cmd)
    (if (null? displayed-value)
	(db-kise/set db-tuple what value)
	(db-kise/set db-tuple what (car displayed-value)))
    ;; updated? reordered?
    (values #t #f)))

(define (db-kise/find-pos tuples what value pred)
  (let ((its-length (length tuples))
	(accessor (if (symbol? what) db-kise/get vector-ref)))
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

(define (db-kise/get-next-id-str)
  "select max(id) from kise where id < '~A';")

(define (db-kise/get-next-id)
  (let* ((next (sqlite/query (db-con)
			     (db-kise/get-next-id-str)))
	 (value (vector-ref (car next) 0)))
    ;; (format #t "db-kise/get-next-id: ~S. value: ~S~%" next value)
    (if value (1+ value) 0)))

(define (db-kise/get-next-id)
  (let* ((delta (aglobs/get 'imported-ids-delta))
	 (query (format #f "~?" (db-kise/get-next-id-str) (list delta)))
	 (tuple (car (sqlite/query (db-con) query)))
	 (max-id (vector-ref tuple 0)))
    ;; if the database is empty, sqlite returns NULL, which in
    ;; guile-sqlite is #f
    (if max-id
	(1+ max-id)
	0)))

(define (db-kise/add-str)
  ;; imported_id has its default set to -1
  ;; imported_db too
  "insert into kise (id,
                     date_,
                     who,
                     for_whom,
                     what,
                     duration,
                     to_be_charged,
                     description,
                     created_the,
                     created_by,
                     modified_the,
                     modified_by,
                     imported_id,
                     imported_db)
   values ('~A',
           strftime('%s','~A'),
           '~A',
           '~A',
           '~A',
           '~A',
           '~A',
           '~A',
           strftime('%s','~A'),
           '~A',
           strftime('%s','~A'),
           '~A',
           '~A',
           '~A')")

(define (db-kise/add date who for-whom what duration to-be-charged description . rests)
  (let* ((next-id (db-kise/get-next-id))
	 (created-the (if (null? rests) date (list-ref rests 0)))
	 (created-by (if (null? rests) who (list-ref rests 1)))
	 (modified-the (if (null? rests) date (list-ref rests 2)))
	 (modified-by (if (null? rests) who (list-ref rests 3)))
	 (imported-id (if (null? rests) -1 (list-ref rests 4)))
	 (imported-db (if (null? rests) -1 (list-ref rests 5)))
	 (insert (format #f "~?" (db-kise/add-str)
			 (list next-id
			       date
			       who
			       for-whom
			       what
			       duration
			       to-be-charged
			       description
			       created-the
			       created-by
			       modified-the
			       modified-by
			       imported-id
			       imported-db))))
    ;; (format #t "~S~%" insert)
    (sqlite/command (db-con) insert)
    next-id))

(define (db-kise/duplicate reference . tuple)
  (if (null? tuple)
      (set! tuple (car (db-kise/select-one reference)))
      (set! tuple (car tuple)))
  ;; date passed to add must be of type iso yyyy-mm-dd. the tuple has
  ;; them like 'dd-mm-yyyy' [till now.
  (let* ((today (date/system-date))
	 (iso-today (date/iso-date today))
	 (date (date/iso-date (db-kise/get tuple 'date_)))
	 (who (str/prep-str-for-sql (db-kise/get tuple 'who))))
    (db-kise/add date
		 who
		 (str/prep-str-for-sql (db-kise/get tuple 'for_whom))
		 (str/prep-str-for-sql (db-kise/get tuple 'what))
		 (db-kise/get tuple 'duration)
		 (db-kise/get tuple 'to_be_charged)
		 (str/prep-str-for-sql (db-kise/get tuple 'description))
		 iso-today
		 who
		 iso-today
		 who
		 -1
		 -1)))

;;;
;;; Import
;;;

(define (db-kise/add-from-other-db-str)
  ;; imported_id has its default set to -1
  ;; imported_db too
  "insert into kise (id,
                     date_,
                     who,
                     for_whom,
                     what,
                     duration,
                     to_be_charged,
                     description,
                     created_the,
                     created_by,
                     modified_the,
                     modified_by,
                     imported_id,
                     imported_db)
   values ('~A','~A','~A','~A','~A','~A','~A','~A','~A','~A','~A','~A','~A','~A')")

(define (db-kise/add-from-other-db id date who for-whom what duration to-be-charged description
				   created-the created-by modified-the modified-by
				   imported-id imported-db-id)
  (let ((insert (format #f "~?" (db-kise/add-from-other-db-str)
			(list id
			      date
			      who
			      for-whom
			      what
			      duration
			      to-be-charged
			      description
			      created-the
			      created-by
			      modified-the
			      modified-by
			      imported-id
			      imported-db-id))))
    ;; (format #t "~S~%" insert)
    (sqlite/command (db-con) insert)
    id))


;;;
;;; Delete
;;;

(define (db-kise/delete-str)
  "delete from kise
    where id = '~A'")

(define (db-kise/delete reference)
  (sqlite/command (db-con)
		  (format #f "~?" (db-kise/delete-str)
			  (list reference))))

(define (db-kise/delete-some-str)
  "delete from kise
    where ~A")

(define* (db-kise/delete-some-1 where)
  (sqlite/command (db-con)
		  (format #f "~?" (db-kise/delete-some-str)
			  (list where))))

(define* (db-kise/delete-some where #:optional (in-transaction? #f))
  (if in-transaction?
      (db-kise/delete-some-1 where)
      (let ((db (db-con)))
	(sqlite/begin-transaction db)
	(db-kise/delete-some-1 where)
	(sqlite/commit db))))


;;;
;;; Import db related stuff
;;;

(define* (db-kise/delete-imported-tuples-1 idb-id delete-imported-db-tuple?)
  (db-kise/delete-some (format #f "imported_db = '~A'" idb-id) #t)
  (when delete-imported-db-tuple? (db-idb/delete idb-id)))

(define* (db-kise/delete-imported-tuples idb-id #:optional (in-transaction? #f) (delete-imported-db-tuple? #f))
  (if in-transaction?
      (db-kise/delete-imported-tuples-1 idb-id delete-imported-db-tuple?)
      (let ((db (db-con)))
	(sqlite/begin-transaction db)
	(db-kise/delete-imported-tuples-1 idb-id delete-imported-db-tuple?)
	(sqlite/commit db))))

(define (db-kise/import-2 tuples idb-id)
  ;; sql transaction must be started by the caller
  (let ((ids-delta (* (1+ idb-id) (aglobs/get 'imported-ids-delta))))
    (for-each (lambda (tuple)
		(let ((imported-id (db-kise/get tuple 'id)))
		  (db-kise/add-from-other-db (+ imported-id ids-delta)
					     (db-kise/get tuple 'date_)
					     (str/prep-str-for-sql (db-kise/get tuple 'who))
					     (str/prep-str-for-sql (db-kise/get tuple 'for_whom))
					     (str/prep-str-for-sql (db-kise/get tuple 'what))
					     (db-kise/get tuple 'duration)
					     (db-kise/get tuple 'to_be_charged)
					     (str/prep-str-for-sql (db-kise/get tuple 'description))
					     (db-kise/get tuple 'created_the)
					     (str/prep-str-for-sql (db-kise/get tuple 'created_by))
					     (db-kise/get tuple 'modified_the)
					     (str/prep-str-for-sql (db-kise/get tuple 'modified_by))
					     imported-id
					     idb-id)))
		tuples)))

(define* (db-kise/import-1 tuples idb-id #:optional (in-transaction? #f))
  (let ((db (db-con)))
    (if in-transaction?
	(db-kise/import-2 tuples idb-id)
	(begin
	  (sqlite/begin-transaction db)
	  (db-kise/import-2 tuples idb-id)
	  (sqlite/commit db)))))

(define* (db-kise/import filename cs-id #:optional (id #f))
  (let* ((uname (sys/get 'uname))
	 (today (date/system-date))
	 (iso-today (date/iso-date today))
	 (idb-tuples (db-idb/select-all))
	 (idb-tuple-pos (db-idb/find-pos idb-tuples 'filename filename string=?))
	 (db (db-con/open filename #f))
	 (tuples (db-kise/select-all-other-db db)))
    (db-con/close db #f)
    (let ((db (db-con)))
      (if idb-tuple-pos
	  (let* ((idb-tuple (list-ref idb-tuples idb-tuple-pos))
		 (idb-id (db-idb/get idb-tuple 'id)))
	    ;; (dimfi "deleting last import..." idb-tuple)
	    (sqlite/begin-transaction db)
	    (db-kise/delete-imported-tuples idb-id #t #f)
	    (db-kise/import-1 tuples idb-id #t)
	    (db-idb/update idb-tuple 'imported_the iso-today)
	    (db-idb/update idb-tuple 'imported_by uname)
	    (sqlite/commit db)
	    idb-id)
	  (let ((idb-id (db-idb/add filename iso-today uname cs-id id)))
	    (sqlite/begin-transaction db)
	    (db-kise/import-1 tuples idb-id #t)
	    (sqlite/commit db)
	    idb-id)))))


#!

(use-modules (kise db-kise))
(reload-module (resolve-module '(kise db-kise)))

(db-con/open "/tmp/new.db")
(db-con/open "/usr/alto/db/sqlite.alto.tests.db")
(db-con/open "/usr/alto/db/sqlite.alto.christian.db")
(sqlite-close (db-con))

(db-kise/get-next-id)

(define tuples (db-kise/select-all))
(define tuple (db-kise/get-tuple tuples 1))

(define tl-widget tl-kwidget*)
(define tuple (ktlw/get-tuple tl-widget 1))

(db-kise/get tuple 'who)
(db-kise/set tuple 'description "another description")
(db-kise/set tuple 'date_ "2012-01-31")

(db-kise/update tuple 'date_ "2010-12-01")
(db-kise/update tuple 'date_ "2010-12-01")

(db-kise/find-pos tuples 'id 40 =)

(db-kise/add "2011-06-14"
	     "david"
	     ""
	     "" 	;; what 
	     0		;; duration
	     "f"	;; to-be-charged
	     "")	;; description

(db-kise/delete "44")
(db-kise/select-some "who = 'david'" #f "sum(duration)")

(db-kise/import "/usr/alto/db/sqlite.alto.christian.db" "#60a8a8" "#000000")
(db-kise/import "/usr/alto/db/sqlite.alto.david.db" "#784800"  "#d8d860")
(db-kise/delete-imported-tuples 0 #f #t)


;;;
;;; SQL examples/tests
;;;

  select * 
    from kise 
   where what like '%email%' 
      or id in (41, 42, 43)
       ;

  select sum(duration)
    from kise 
   where for_whom like '%lpdi%' 
      or id in (100)
       ;

  select sum(duration)
    from kise 
   where for_whom like '%lpdi%'
     and to_be_charged = 't'
       ;

;; this filter

  select *
    from kise 
   where for_whom like '%lpdi%'
     and to_be_charged = 't'
      or id in (100, 101, 102)
       ;

;; can be used ^^ for the total time
;; --> BUT will need this query, below, for the charged time

  select sum(duration)
    from kise 
   where (for_whom like '%lpdi%' or id in (100, 101, 102))
     and to_be_charged = 't'
       ;

 update kise
    set date_ = '1246060800'
  where id = '40';

select id, strftime('%d.%m.%Y', date_, 'unixepoch') as date_ from kise;

!#

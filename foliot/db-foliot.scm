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


(define-module (foliot db-foliot)
  #:use-module (ice-9 format)
  #:use-module (ice-9 receive)
  #:use-module (oop goops)
  #:use-module (grip module)
  #:use-module (grip iter)
  #:use-module (grip date)
  #:use-module (grip i18n)
  #:use-module (grip utils)
  #:use-module (grip passwd)
  #:use-module (grip string)
  #:use-module (grip sqlite)
  #:use-module (grip gnome color)
  #:use-module (foliot globals)
  #:use-module (foliot db-con)
  #:use-module (foliot db-imported-db)
  
  #:export (db-foliot/add-foliot-table
	    db-foliot/complete-table
	    db-foliot/create-complete-table
	    db-foliot/prepend-empty
	    db-foliot/select-one
	    db-foliot/select-all
	    db-foliot/select-all-other-db
	    db-foliot/select-some
	    db-foliot/select-another-some
	    db-foliot/select-distinct
	    db-foliot/get-pos
	    db-foliot/get
	    db-foliot/set
	    db-foliot/get-tuple
	    db-foliot/update
	    db-foliot/find-pos
	    db-foliot/get-next-id
	    db-foliot/add
	    db-foliot/add-from-other-db
	    db-foliot/duplicate
	    db-foliot/delete
	    db-foliot/delete-some
	    db-foliot/import
	    db-foliot/delete-imported-tuples))


(eval-when (expand load eval)
  (re-export-public-interface (grip sqlite)
			      (grip date)
			      (grip i18n)
			      (grip utils)
			      (grip passwd)
			      (grip string)
			      (foliot globals)
			      (foliot db-con)
			      (foliot db-imported-db))
  (textdomain "db-foliot")
  (bindtextdomain "db-foliot" (ref %foliot-store 'pofdir)))


;;;
;;; Globals
;;;

(define (db-foliot/fields)
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

(define (db-foliot/fields-offsets)
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

(define (db-foliot/get-pos what)
  (assq-ref (db-foliot/fields-offsets) what))

(define (db-foliot/get db-tuple what)
  ;; db-tuple is a vector. NULL values are returned as #f by sqlite.scm
  ;; which is a problem if the field is used in _text_ widget
  (let ((value (vector-ref db-tuple (db-foliot/get-pos what))))
    (case what
      ((date date_) (if value value ""))
      (else value))))

(define (db-foliot/set db-tuple what value)
  ;; db-tuple is a vector
  (vector-set! db-tuple (db-foliot/get-pos what) value))

(define (db-foliot/get-tuple tuples offset)
  ;; so far, tuples is a list
  (list-ref tuples offset))


;;;
;;; Non api stuff
;;;

(define (db-foliot/prepend-empty tuples . v-size)
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

(define (db-foliot/add-foliot-table-str)
  "create table foliot (
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

(define (db-foliot/add-foliot-table db)
  (sqlite/command db (db-foliot/add-foliot-table-str)))

(define (db-foliot/complete-table db)
  (let ((table-info (sqlite/table-info db "foliot")))
    (unless (sqlite/tuple-pos "imported_id" table-info string=? 1)
      ;; upgrading from 0.9.1 to 0.9.2
      (sqlite/add-column db "foliot" "imported_id integer default '-1' not null")
      (sqlite/add-column db "foliot" "imported_db integer default '-1' not null"))))

(define (db-foliot/create-complete-table db)
  (if (sqlite/table-exists? db "foliot")
      (db-foliot/complete-table db)
      (db-foliot/add-foliot-table db)))


;;;
;;; Select
;;;

(define (db-foliot/select-one-str)
  "select ~A
     from foliot
    where id='~A'")

(define (db-foliot/select-one reference)
  (sqlite/query (db-con)
		(format #f "~?" (db-foliot/select-one-str)
			(list (db-foliot/fields)
			      reference))))

(define %order-by-str
  "date_ desc, who asc, for_whom asc, what asc, to_be_charged asc, id desc")

(define (db-foliot/select-all-str)
  "select ~A
     from foliot
 order by ~A")

(define (db-foliot/select-all . aggregate?)
  (let ((what (if (null? aggregate?) (db-foliot/fields) (car aggregate?))))
    (sqlite/query (db-con)
		  (format #f "~?" (db-foliot/select-all-str)
			  (list what
				%order-by-str)))))

(define %select-all-foliot-tuples-str
  ;; do not process dates, they will be imported as is
  "select *
     from foliot
 order by ~A")

(define %select-all-kise-tuples-str
  ;; do not process dates, they will be imported as is
  "select *
     from kise
 order by ~A")

(define (db-foliot/select-all-other-db db)
  (sqlite/query db
		(format #f "~?"
			;; see the comment we wrote for db-foliot/import
			(if (sqlite/table-exists? db "kise")
			    %select-all-kise-tuples-str
			    %select-all-foliot-tuples-str)
			(list %order-by-str))))

(define (db-foliot/select-some-str)
  "select ~A
     from foliot
    where ~A
 order by ~A")

(define (db-foliot/select-some-with-ids-str)
  "select ~A
     from foliot
    where ~A
       or id in ~A
 order by ~A")

(define (db-foliot/select-some where ids . aggregate?)
  (let ((what (if (null? aggregate?) (db-foliot/fields) (car aggregate?))))
    (cond ((and where ids)
	   (sqlite/query (db-con)
			 (format #f "~?" (db-foliot/select-some-with-ids-str)
				 (list what
				       where
				       (sqlite/build-set-expression ids)
				       %order-by-str))))
	  (where
	   (sqlite/query (db-con)
			 (format #f "~?" (db-foliot/select-some-str)
				 (list what
				       where
				       %order-by-str))))
	  (else
	   (if (null? aggregate?)
	       (db-foliot/select-all)
	       (db-foliot/select-all (car aggregate?)))))))

(define (db-foliot/select-another-some-str mode)
  (case mode
    ((1) "select ~A from foliot where ~A group by ~A order by ~A")
    ((2) "select ~A from foliot where ~A group by ~A")
    ((3) "select ~A from foliot where ~A")
    ((4) "select ~A from foliot where ~A order by ~A")
    ((5) "select ~A from foliot group by ~A order by ~A")
    ((6) "select ~A from foliot group by ~A")
    ((7) "select ~A from foliot order by ~A")
    ((8) "select ~A from foliot")))

(define (db-foliot/select-another-some where group-by order-by)
  (let* ((mode (cond ((and where group-by order-by) 1)
		     ((and where group-by) 2)
		     ((and where order-by) 4)
		     (where 3)
		     ((and group-by order-by) 5)
		     (group-by 6)
		     (order-by 7)
		     (else
		      8)))
	 (query-string (format #f "~?" (db-foliot/select-another-some-str mode)
			       (case mode
				 ((1) (list (db-foliot/fields) where group-by order-by))
				 ((2) (list (db-foliot/fields) where group-by))
				 ((3) (list (db-foliot/fields) where))
				 ((4) (list (db-foliot/fields) where order-by))
				 ((5) (list (db-foliot/fields) group-by order-by))
				 ((6) (list (db-foliot/fields) group-by))
				 ((7) (list (db-foliot/fields) order-by))
				 ((8) (list (db-foliot/fields)))))))
    ;; (format #t "db-foliot/select-another-some~%  ~S~%" query-string)
    (sqlite/query (db-con) query-string)))

(define (db-foliot/select-distinct-str)
  "select distinct(~A) from foliot order by ~A;")

(define (db-foliot/select-distinct colname . add-empty?)
  ;; because the results of this function is also used to build combo,
  ;; we need the ability to add an empty entry id necessary.
  (let ((distincts (sqlite/query (db-con)
				 (format #f "~?" (db-foliot/select-distinct-str)
					 (list colname colname)))))
    (if (null? add-empty?)
	distincts
	(db-foliot/prepend-empty distincts))))


;;;
;;; Updates
;;;

(define (db-foliot/set-date-str)
  "update foliot
      set ~A = strftime('%s','~A')
    where id = '~A';")

(define (db-foliot/set-str)
  "update foliot
      set ~A = '~A'
    where id = '~A';")

(define (db-foliot/update db-tuple what value . displayed-value)
  (let* ((id (db-foliot/get db-tuple 'id))
	 (sql-value (case what
		      ((who for_whom what description) (string-escape-sql value))
		      (else
		       value)))
	 (sql-str (case what
		    ((date_ created_the modified_the) (db-foliot/set-date-str))
		    (else
		     (db-foliot/set-str))))
	 (cmd (format #f "~?" sql-str (list what sql-value id))))
    ;; (format #t "~S~%Displayed value: ~S~%" cmd displayed-value)
    (sqlite/command (db-con) cmd)
    (if (null? displayed-value)
	(db-foliot/set db-tuple what value)
	(db-foliot/set db-tuple what (car displayed-value)))
    ;; updated? reordered?
    (values #t #f)))

(define (db-foliot/find-pos tuples what value pred)
  (let ((its-length (length tuples))
	(accessor (if (symbol? what) db-foliot/get vector-ref)))
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

(define (db-foliot/get-next-id-str)
  "select max(id) from foliot where id < '~A';")

(define (db-foliot/get-next-id)
  (let* ((next (sqlite/query (db-con)
			     (db-foliot/get-next-id-str)))
	 (value (vector-ref (car next) 0)))
    ;; (format #t "db-foliot/get-next-id: ~S. value: ~S~%" next value)
    (if value (1+ value) 0)))

(define (db-foliot/get-next-id)
  (let* ((delta (ref %foliot-store 'imported-ids-delta))
	 (query (format #f "~?" (db-foliot/get-next-id-str) (list delta)))
	 (tuple (car (sqlite/query (db-con) query)))
	 (max-id (vector-ref tuple 0)))
    ;; if the database is empty, sqlite returns NULL, which in
    ;; guile-sqlite is #f
    (if max-id
	(1+ max-id)
	0)))

(define (db-foliot/add-str)
  ;; imported_id has its default set to -1
  ;; imported_db too
  "insert into foliot (id,
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

(define (db-foliot/add date who for-whom what duration to-be-charged description . rests)
  (let* ((next-id (db-foliot/get-next-id))
	 (created-the (if (null? rests) date (list-ref rests 0)))
	 (created-by (if (null? rests) who (list-ref rests 1)))
	 (modified-the (if (null? rests) date (list-ref rests 2)))
	 (modified-by (if (null? rests) who (list-ref rests 3)))
	 (imported-id (if (null? rests) -1 (list-ref rests 4)))
	 (imported-db (if (null? rests) -1 (list-ref rests 5)))
	 (insert (format #f "~?" (db-foliot/add-str)
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

(define (db-foliot/duplicate reference . tuple)
  (if (null? tuple)
      (set! tuple (car (db-foliot/select-one reference)))
      (set! tuple (car tuple)))
  ;; date passed to add must be of type iso yyyy-mm-dd. the tuple has
  ;; them like 'dd-mm-yyyy' [till now.
  (let* ((today (date/system-date))
	 (iso-today (date/iso-date today))
	 (date (date/iso-date (db-foliot/get tuple 'date_)))
	 (username (sys/get 'uname))
	 (who (string-escape-sql (db-foliot/get tuple 'who))))
    (db-foliot/add date
		 who
		 (string-escape-sql (db-foliot/get tuple 'for_whom))
		 (string-escape-sql (db-foliot/get tuple 'what))
		 (db-foliot/get tuple 'duration)
		 (db-foliot/get tuple 'to_be_charged)
		 (string-escape-sql (db-foliot/get tuple 'description))
		 iso-today
		 username
		 iso-today
		 username
		 -1
		 -1)))

;;;
;;; Import
;;;

(define (db-foliot/add-from-other-db-str)
  ;; imported_id has its default set to -1
  ;; imported_db too
  "insert into foliot (id,
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

(define (db-foliot/add-from-other-db id date who for-whom what duration to-be-charged description
				   created-the created-by modified-the modified-by
				   imported-id imported-db-id)
  (let ((insert (format #f "~?" (db-foliot/add-from-other-db-str)
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

(define (db-foliot/delete-str)
  "delete from foliot
    where id = '~A'")

(define (db-foliot/delete reference)
  (sqlite/command (db-con)
		  (format #f "~?" (db-foliot/delete-str)
			  (list reference))))

(define (db-foliot/delete-some-str)
  "delete from foliot
    where ~A")

(define* (db-foliot/delete-some-1 where)
  (sqlite/command (db-con)
		  (format #f "~?" (db-foliot/delete-some-str)
			  (list where))))

(define* (db-foliot/delete-some where #:optional (in-transaction? #f))
  (if in-transaction?
      (db-foliot/delete-some-1 where)
      (let ((db (db-con)))
	(sqlite/begin-transaction db)
	(db-foliot/delete-some-1 where)
	(sqlite/commit db))))


;;;
;;; Import db related stuff
;;;

(define* (db-foliot/delete-imported-tuples-1 idb-id delete-imported-db-tuple?)
  (db-foliot/delete-some (format #f "imported_db = '~A'" idb-id) #t)
  (when delete-imported-db-tuple? (db-idb/delete idb-id)))

(define* (db-foliot/delete-imported-tuples idb-id #:optional (in-transaction? #f) (delete-imported-db-tuple? #f))
  (if in-transaction?
      (db-foliot/delete-imported-tuples-1 idb-id delete-imported-db-tuple?)
      (let ((db (db-con)))
	(sqlite/begin-transaction db)
	(db-foliot/delete-imported-tuples-1 idb-id delete-imported-db-tuple?)
	(sqlite/commit db))))

(define (db-foliot/import-2 tuples idb-id)
  ;; sql transaction must be started by the caller
  (let ((ids-delta (* (1+ idb-id) (ref %foliot-store 'imported-ids-delta))))
    (for-each (lambda (tuple)
		(let ((imported-id (db-foliot/get tuple 'id)))
		  (db-foliot/add-from-other-db (+ imported-id ids-delta)
					     (db-foliot/get tuple 'date_)
					     (string-escape-sql (db-foliot/get tuple 'who))
					     (string-escape-sql (db-foliot/get tuple 'for_whom))
					     (string-escape-sql (db-foliot/get tuple 'what))
					     (db-foliot/get tuple 'duration)
					     (db-foliot/get tuple 'to_be_charged)
					     (string-escape-sql (db-foliot/get tuple 'description))
					     (db-foliot/get tuple 'created_the)
					     (string-escape-sql (db-foliot/get tuple 'created_by))
					     (db-foliot/get tuple 'modified_the)
					     (string-escape-sql (db-foliot/get tuple 'modified_by))
					     imported-id
					     idb-id)))
		tuples)))

(define* (db-foliot/import-1 tuples idb-id #:optional (in-transaction? #f))
  (let ((db (db-con)))
    (if in-transaction?
	(db-foliot/import-2 tuples idb-id)
	(begin
	  (sqlite/begin-transaction db)
	  (db-foliot/import-2 tuples idb-id)
	  (sqlite/commit db)))))

(define* (db-foliot/import filename cs-id #:optional (id #f) (idb #f))
  (let* ((uname (sys/get 'uname))
	 (today (date/system-date))
	 (iso-today (date/iso-date today))
	 (idb-tuples (db-idb/select-all))
	 (idb-tuple-pos (db-idb/find-pos idb-tuples 'filename filename string=?))
	 (idb-con (or idb
		      ;; Caution: when idb is not #f, the schema checks for this db have been
		      ;; done and successful.  This is not the case if idb is #f of course.
		      ;; Now, this procedure is also called for re importing db, and it could
		      ;; be an old kise-* schema.  We can either import the kise table or ask
		      ;; the user to upgrade that particular db, then re importing it again.
		      ;; We'll do the former, because it is so much easier for the user, and
		      ;; there has been no change between the kise and the foliot table
		      ;; definition since Kisê -> GNU Foliot, so we are on the safe side doing
		      ;; this, until the foliot table changes, this solution is ok.
		      (db-con/open filename #f)))
	 (tuples (db-foliot/select-all-other-db idb-con)))
    (db-con/close idb-con #f)
    (let ((db (db-con)))
      (if idb-tuple-pos
	  (let* ((idb-tuple (list-ref idb-tuples idb-tuple-pos))
		 (idb-id (db-idb/get idb-tuple 'id)))
	    ;; (dimfi "deleting last import..." idb-tuple)
	    (sqlite/begin-transaction db)
	    (db-foliot/delete-imported-tuples idb-id #t #f)
	    (db-foliot/import-1 tuples idb-id #t)
	    (db-idb/update idb-tuple 'imported_the iso-today)
	    (db-idb/update idb-tuple 'imported_by uname)
	    (sqlite/commit db)
	    idb-id)
	  (let ((idb-id (db-idb/add filename iso-today uname cs-id id)))
	    (sqlite/begin-transaction db)
	    (db-foliot/import-1 tuples idb-id #t)
	    (sqlite/commit db)
	    idb-id)))))


#!

(db-con/open "/tmp/new.db")
(db-con/open "/usr/alto/db/sqlite.alto.tests.db")
(db-con/open "/usr/alto/db/sqlite.alto.christian.db")
(sqlite-close (db-con))

(db-foliot/get-next-id)

(define tuples (db-foliot/select-all))
(define tuple (db-foliot/get-tuple tuples 1))

(define tl-widget tl-kwidget*)
(define tuple (ftlw/get-tuple tl-widget 1))

(db-foliot/get tuple 'who)
(db-foliot/set tuple 'description "another description")
(db-foliot/set tuple 'date_ "2012-01-31")

(db-foliot/update tuple 'date_ "2010-12-01")
(db-foliot/update tuple 'date_ "2010-12-01")

(db-foliot/find-pos tuples 'id 40 =)

(db-foliot/add "2011-06-14"
	     "david"
	     ""
	     "" 	;; what 
	     0		;; duration
	     "f"	;; to-be-charged
	     "")	;; description

(db-foliot/delete "44")
(db-foliot/select-some "who = 'david'" #f "sum(duration)")

(db-foliot/import "/usr/alto/db/sqlite.alto.christian.db" "#60a8a8" "#000000")
(db-foliot/import "/usr/alto/db/sqlite.alto.david.db" "#784800"  "#d8d860")
(db-foliot/delete-imported-tuples 0 #f #t)


;;;
;;; SQL examples/tests
;;;

  select * 
    from foliot 
   where what like '%email%' 
      or id in (41, 42, 43)
       ;

  select sum(duration)
    from foliot 
   where for_whom like '%lpdi%' 
      or id in (100)
       ;

  select sum(duration)
    from foliot 
   where for_whom like '%lpdi%'
     and to_be_charged = 't'
       ;

;; this filter

  select *
    from foliot 
   where for_whom like '%lpdi%'
     and to_be_charged = 't'
      or id in (100, 101, 102)
       ;

;; can be used ^^ for the total time
;; --> BUT will need this query, below, for the charged time

  select sum(duration)
    from foliot 
   where (for_whom like '%lpdi%' or id in (100, 101, 102))
     and to_be_charged = 't'
       ;

 update foliot
    set date_ = '1246060800'
  where id = '40';

select id, strftime('%d.%m.%Y', date_, 'unixepoch') as date_ from foliot;

!#

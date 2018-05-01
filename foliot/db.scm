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


(define-module (foliot db)
  #:use-module (grip module)
  #:use-module (grip sqlite)
  #:use-module (foliot db-con)
  #:use-module (foliot db-foliot)
  #:use-module (foliot db-imported-db)
  #:use-module (foliot db-printing-templates)
  #:use-module (foliot db-shinning)

  #:export (db/add-schema
	    db/check-schema
	    db/complete-schema))


(eval-when (expand load eval)
  (re-export-public-interface (grip sqlite)
			      (foliot db-con)
			      (foliot db-foliot)
			      (foliot db-imported-db)
			      (foliot db-printing-templates)
			      (foliot db-shinning)))


;;;
;;; Schema
;;;

(define (db/add-schema db)
  (db-foliot/add-foliot-table db)
  (db-pt/add-printing-templates-table db)
  (db-idb/add-imported-db-table db)
  (db-shi/add-shinning-table db))

(define (db/check-schema db)
  ;; February the 9th, 2016, Kisê becomes GNU Foliot.  Unless the database
  ;; was created using GNU Foliot, we need to rename tables from kise- to
  ;; foliot-, then proceed with the usual schema checks.  Note that the
  ;; schema could still be incomplete, for example users of very early
  ;; versions of this app, then start to use it again... So we need to
  ;; test each table existence individually.
  (when (sqlite/table-exists? db "kise")
    (sqlite/begin-transaction db)
    (sqlite/table-rename db "kise" "foliot")
    (sqlite/commit db))
  (when (sqlite/table-exists? db "kise_printing_templates")
    (sqlite/begin-transaction db)
    (sqlite/table-rename db "kise_printing_templates" "foliot_printing_templates")
    (sqlite/commit db))
  (when (sqlite/table-exists? db "kise_imported_db")
    (sqlite/begin-transaction db)
    (sqlite/table-rename db "kise_imported_db" "foliot_imported_db")
    (sqlite/commit db))
  (when (sqlite/table-exists? db "kise_shinning")
    (sqlite/begin-transaction db)
    (sqlite/table-rename db "kise_shinning" "foliot_shinning")
    (sqlite/commit db))
  ;; Caution: table names are cached, we must refresh!
  (sqlite/table-names db #:refresh #t)
  (let ((foliot? (sqlite/table-exists? db "foliot"))
	(foliot-printing-templates? (sqlite/table-exists? db "foliot_printing_templates"))
	(foliot-imported-db? (db-idb/check-schema db))
	(foliot-shinning? (db-shi/check-schema db)))
    (cond ((and foliot? ;; not good yet we should still check the defs
		foliot-printing-templates? ;; not good yet we should still check the defs
		(eq? foliot-imported-db? 'complete)
		(eq? foliot-shinning? 'complete))
	   'complete)
	  ((or foliot?
	       foliot-printing-templates?
	       (eq? foliot-imported-db? 'partial)
	       (eq? foliot-shinning? 'partial))
	   'partial)
	  (else
	   'none))))

(define (db/complete-schema db)
  (db-foliot/create-complete-table db)
  (db-pt/create-complete-table db)
  (db-idb/create-complete-table db)
  (db-shi/create-complete-table db)
  (sqlite/table-names db #:refresh #t))

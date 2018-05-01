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

;; Notes: the idea is to store the minimum possible infos in the
;; user's foliot.conf file. This means, once the application has been
;; used at least once, the db filename and whether it should be used
;; at startup [not offering the db connection dialog at startup], or
;; not. The rest of user's config, preferences and hopefully the undo
;; system will be stored in the database itself.

;;; Code:


(define-module (foliot config)
  #:use-module (grip module)
  #:use-module (grip passwd)
  #:use-module (grip config)

  #:export (fcfg/get))


(define fcfg/get #f)


(eval-when (expand load eval)
  (re-export-public-interface (grip passwd)
			      (grip config))
  (let ((config (read-config "foliot")))
    (set! fcfg/get
	  (lambda (what)
	    (case what
	      ((all)
	       config)
	      ((reload)
	       (set! config (read-config "foliot"))
	       config)
	      (else
	       (assq-ref config what)))))))

;; the solution here below, which was suggested by mark weaver on irc,
;; would work @ compile load eval time, but not @ expand tine.  till
;; now, I don't need any of the GNU Foliot's config user setting at expand
;; time, but that might not always be true.  i keep the definition
;; below as an example, it could even help someone else :lo:.

#;(define fcfg/get
  (let ((config (read-config "foliot")))
    (lambda (what)
      (case what
	((all)
	 config)
	((reload)
	 (read-config "foliot")
	 config)
	(else
	 (assq-ref config what))))))


#!

(fcfg/get 'all)
(fcfg/get 'reload)
(fcfg/get 'db-file)
(fcfg/get 'open-at-startup)
(fcfg/get 'ulogo)
(fcfg/get 'win-x)
(fcfg/get 'win-y)
(fcfg/get 'win-w)
(fcfg/get 'win-h)

!#


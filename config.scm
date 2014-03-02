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

;; Notes: the idea is to store the minimum possible infos in the
;; user's kise.conf file. This means, once the application has been
;; used at least once, the db filename and whether it should be used
;; at startup [not offering the db connection dialog at startup], or
;; not. The rest of user's config, preferences and hopefully the undo
;; system will be stored in the database itself.

;;; Code:

(define-module (kise config)
  ;; guile
  :use-module (ice-9 format)
  :use-module (oop goops)

  ;; common
  :use-module (macros reexport)
  :use-module (system passwd)
  :use-module (system config)

  :export (kcfg/get))


(define *config* #f)

(eval-when (compile load eval)
  (set! *config* (sys/read-config "kise"))
  (re-export-public-interface (ice-9 format)
			      (oop goops)
			      (system passwd)
			      (system config)))

(define (kcfg/get what)
  (case what
    ((reload) (set! *config* (sys/read-config "kise")))
    (else
     (let ((pair (and *config* (assoc what *config*))))
       (and pair (cdr pair))))))


#!

(use-modules (kise config))
(reload-module (resolve-module '(kise config)))

(kcfg/get 'db-file)
(kcfg/get 'open-at-startup)
(kcfg/get 'ulogo)
(kcfg/get 'win-x)
(kcfg/get 'win-y)
(kcfg/get 'win-w)
(kcfg/get 'win-h)


;;;
;;;
;;;

(define istream (open-input-file (string-append (sys/get 'udir) "/.config/kise.conf")))
(define line (read istream))

!#


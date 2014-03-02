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

;; this module is left for a future release

;;; Code:

(define-module (kise p-commercial)
  ;; guile/guile-gnome
  :use-module (oop goops)
  ;; :use-module (gnome gw generics)

  ;; common

  ;; kise

  :export (kp/print-commercial))


;;;
;;; Globals
;;;


;;;
;;; API
;;;

(define (kp/print-commercial kp/widget tl-widget tex-files)
  (format #t "kp/print-commercial not implemented yet.~%")
  #t)


#!

(use-modules (kise p-commercial))
(reload-module (resolve-module '(kise p-commercial)))

!#

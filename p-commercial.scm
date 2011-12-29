;; -*- mode: scheme; coding: utf-8 -*-

;;;; Copyright (C) 2011, 2012
;;;; Free Software Foundation, Inc.
;;;;
;;;; This library is free software; you can redistribute it and/or
;;;; modify it under the terms of the GNU Lesser General Public
;;;; License as published by the Free Software Foundation; either
;;;; version 3 of the License, or (at your option) any later version.
;;;; 
;;;; This library is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;;; Lesser General Public License for more details.
;;;; 
;;;; You should have received a copy of the GNU Lesser General Public
;;;; License along with this library; if not, write to the Free Software
;;;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
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

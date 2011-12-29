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

;; Since goops has been debugged, see :duplicates (merge-generics
;; ...), we don't need such generics files anymore. We keep it here as
;; part of some sort of history of this project.

;;; Code:

(define-module (kise generics)
  ;; guile
  :use-module (oop goops)

  :export (dialog	;; <kp/widget>
	   pdf
	   items
	   mode
	   grouping))

;;;
;;; p-dialog
;;;

(define-generic dialog)
(define-generic pdf)
(define-generic items)
(define-generic mode)
(define-generic grouping)



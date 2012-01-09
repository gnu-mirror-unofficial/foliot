;; -*- mode: scheme; coding: utf-8 -*-

;;;; Copyright (C) 2011, 2012
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

;; This is reaaaally an on going work - Do not even read it lol.

;;; Code:

(define-module (kise ogwanda tree-ops)
    :export (tree/add))

(define (tree/add tree pathname)
  ;; we don't check the validity
  (let* ((path-items (cdr (string-split item  #\/)))
	 (its-length (length path-items)))
    ;; so, we look for the path in the gtktree
    ;; or add requested nodes so that this path exists
    ;; at the end of the execution of this function
    #t))


#!

(use-modules (kise ogwanda tree-ops))
(reload-module (resolve-module '(kise ogwanda tree-ops)))

(define tree '(a (b) (c (d)) (e)))

(tree/find-subtree tree 'c)

(define activities
  '("/admin/email/tp5e-3"
    "/admin/tp5e-3"))

(define tree
  ;; no root
  '((admin (acounting)
    (sysadmin (install (slicer4 (asterix idefix)))
	      (update (slicer4 (asterix idefix))))
    (devel (kise))
    ;; ...
    )))

!#

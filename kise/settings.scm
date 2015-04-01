;; -*- mode: scheme; coding: utf-8 -*-

;;;; Copyright (C) 2015
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

;; guild compile does not let us set guile's global settings, neither
;; specific guile's core modules global settings such as merge-generics
;; (oop goops).  These settings, by definitin, are set once only and
;; generally this is done in the main application script.  To be able to
;; adequately integrate with the autotools chain, we have to use guild
;; compile, so need to work around this limitation.  Hence this module,
;; which must be imported where we or the system defines/imports
;; generic functions ...

;;; Code:


(define-module (kise settings)
  #:use-module (oop goops))


(eval-when (expand load eval)
  (default-duplicate-binding-handler
    '(merge-generics replace warn-override-core warn last)))

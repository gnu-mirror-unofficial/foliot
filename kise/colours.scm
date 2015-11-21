;; -*- mode: scheme; coding: utf-8 -*-

;;;;
;;;; Copyright (C) 2011 - 2015

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


(define-module (kise colours)
  #:use-module (grip reexport)
  #:use-module (grip gnome colours)

  #:export (*filters-fg*
	    *filters-border*
	    *filters-bg*
	    *dialog-title-eb-bg*))


(eval-when (expand load eval)
  (re-export-public-interface (grip gnome colours)))


(define *filters-bg* (colour-set-bg 60))
(define *filters-border* (colour-set-border 60))
(define *filters-fg* (colour-set-fg 60))
(define *dialog-title-eb-bg* (colour-set-bg 50))


#!

;; Gtk widget states

'((normal GTK_STATE_NORMAL 0)
  (active GTK_STATE_ACTIVE 1)
  (prelight GTK_STATE_PRELIGHT 2) 
  (selected GTK_STATE_SELECTED 3)
  (insensitive GTK_STATE_INSENSITIVE 4))

!#

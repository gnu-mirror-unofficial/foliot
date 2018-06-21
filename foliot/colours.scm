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


(define-module (foliot colours)
  #:use-module (grip module)
  #:use-module (grip gnome color)

  #:export (*filters-fg*
	    *filters-border*
	    *filters-bg*
	    *dialog-title-eb-bg*))


(eval-when (expand load eval)
  (re-export-public-interface (grip gnome color)))


(define *filters-bg* (color-set-bg 60))
(define *filters-border* (color-set-border 60))
(define *filters-fg* (color-set-fg 60))
(define *dialog-title-eb-bg* (color-set-bg 50))


#!

;; Gtk widget states

'((normal GTK_STATE_NORMAL 0)
  (active GTK_STATE_ACTIVE 1)
  (prelight GTK_STATE_PRELIGHT 2) 
  (selected GTK_STATE_SELECTED 3)
  (insensitive GTK_STATE_INSENSITIVE 4))

!#

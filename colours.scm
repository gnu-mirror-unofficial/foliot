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

;;; Code:

(define-module (kise colours)
  :use-module (gnome gtk)

  :export (*kc/filters-fg*
	   *kc/filters-bg*
	   *kc/dialog-title-eb-bg*))


;;;
;;; Palette
;;;

;; Persistance of memory [S. Dali]

(define *pom/dirt* "#784800") ;; maron
(define *pom/dirt-border* "#6b4100")

(define *pom/retroflowers* "#d8d860") ;; jaune 'sale'
(define *pom/retroflowers-border* "#c6c352")

(define *pom/sobe* "#60a8a8") ;; bleu 'turquoise'
(define *pom/sobe-border* "#529694")

(define *pom/bittersweet* "#483000") ;; brun
(define *pom/bittersweet-border* "#422800")

(define *pom/aquanight* "#90c0a8") ;; bleu 'pale' 'gris/vert'
(define *pom/aquanight-border* "#84ae94")


;;;
;;; Kise globals
;;;

;; (define *kc/filters-fg* "#91571c") ;; light brown
(define *kc/filters-fg* "#2f4f2f") ;; dark green
(define *kc/filters-bg* "#aabbaa") ;; pale green

;; (define *kc/dialog-title-eb-bg* "#e1eee1")
;; (define *kc/dialog-title-eb-bg* "LightYellow2")
(define *kc/dialog-title-eb-bg* "LightYellow3")
;; (define *kc/dialog-title-eb-bg* "Aquamarine3")
;; (define *kc/dialog-title-eb-bg* "LightSalmon1")
;; (define *kc/dialog-title-eb-bg* "DarkKhaki")
;; (define *kc/dialog-title-eb-bg* "MediumPurple")
;; (define *kc/dialog-title-eb-bg* "LightSteelBlue")



#!

(use-modules (kise colours))
(reload-module (resolve-module '(kise colours)))


;; Gtk widget states

'((normal GTK_STATE_NORMAL 0)
  (active GTK_STATE_ACTIVE 1)
  (prelight GTK_STATE_PRELIGHT 2) 
  (selected GTK_STATE_SELECTED 3)
  (insensitive GTK_STATE_INSENSITIVE 4))

!#
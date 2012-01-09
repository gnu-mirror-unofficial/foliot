#!/bin/sh
# -*- mode: scheme; coding: utf-8 -*-
export G_SLICE=always-malloc
export GUILE_WARN_DEPRECATED="detailed"
# ( cd `dirname $0` && make `basename $0` >/dev/null ) || exit
exec guile-gnome-2 -e main -s $0 "$@"
!#

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

;;; Code:

(read-set! keywords 'prefix)
(use-modules (ice-9 getopt-long)
	     ;; common
	     (system passwd)
	     (system i18n)
	     (system aglobs)
	     (kise globals))


(eval-when (compile)
  (primitive-eval '(use-modules (oop goops)
				(gnome gnome)
				(gnome gnome-ui)
				(gnome gtk)
				(gnome gtk graphical-repl)
				(gnome glib))))


;;;
;;; Globals
;;;

(eval-when (compile load eval)
  (aglobs/set 'command-synopsis
	      '((version   (single-char #\v) (value #f))
		(help      (single-char #\h) (value #f))
		(debug     (single-char #\d) (value #f))))
  (aglobs/set 'version "0.9")
  (textdomain "main")
  (bindtextdomain "main" (aglobs/get 'pofdir)))


;;;
;;; Usage and other display
;;;

(define (display-version port)
  ;; 'GNU Kise', really nice! But let's wait GNU evaluation/acceptance
  ;; (aglobs/display (string-append "GNU Kise " (aglobs/get 'version)) port)
  (aglobs/display (string-append "Kise " (aglobs/get 'version)) port))

(define (display-help port)
  (aglobs/display "Usage: kise [options...]" port)
  (aglobs/display "  --help, -h        Show this usage information" port)
  (aglobs/display "  --version, -v     Show version information" port)
  (aglobs/display "  --debug, -d       Open a debugger aside the application" port))


;;;
;;; Main
;;;

(define (main args)
  (let* ((options (getopt-long args (aglobs/get 'command-synopsis)))
	 (help? (option-ref options 'help #f))
	 (version? (option-ref options 'version #f))
	 (debug? (option-ref options 'debug #f))
	 (port (current-output-port))
	 (uname (sys/get 'uname))
	 (version (aglobs/get 'version)))
    (set-port-encoding! port "utf-8")
    (cond (version? (display-version port))
	  (help? (display-help port))
	  (else
	   (aglobs/set 'debug #t)
	   (catch #t
	     (lambda () (setlocale LC_ALL "")) ;; gettext will not work otherwise
	     (lambda args #f))
	   (display-version port)
	   (aglobs/display (string-append "  " (_ "loading core modules") "...") port)
	   (primitive-eval '(use-modules (oop goops)
					 (gnome gnome)
					 (gnome gnome-ui)
					 (gnome gtk)
					 (gnome gtk graphical-repl)
					 (gnome glib)))
	   (aglobs/display (string-append "  " (_ "loading Kise") "...") port)
	   (gtk-rc-parse (aglobs/get 'gtkrcfile))
	   (primitive-eval '(use-modules (kise kise)))
	   (load-user-init)
	   (gnome-program-init "Kise" version)
	   (gnome-authentication-manager-init)
	   (aglobs/display (string-append "  " (_ "animating") "...") port)
	   (kise/animate-ui uname (aglobs/get 'gladefile) version debug?)
	   (if debug?
	       (guile-gtk-repl)
	       (g-main-loop-run (g-main-loop-new)))))))

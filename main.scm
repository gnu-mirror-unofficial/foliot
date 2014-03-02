#!/bin/sh
# -*- mode: scheme; coding: utf-8 -*-
export G_SLICE=always-malloc
export GUILE_WARN_DEPRECATED="detailed"
# ( cd `dirname $0` && make `basename $0` >/dev/null ) || exit
exec guile-gnome-2 -e main -s $0 "$@"
!#

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

(read-set! keywords 'prefix)


;;;
;;; 1st of a 2 steps load modules
;;;    [ to handle the locale 'problem' ]
;;;

(use-modules (ice-9 getopt-long)
	     (system locale) ;; re-export aglobs
	     (system passwd)
	     (system i18n)
	     (kise globals))


;;;
;;; Globals and locale
;;;

(define (copyright-message)
  "Copyright (C) 2011, 2012, 2013  Free Software Foundation, Inc.

Kise comes with ABSOLUTELY NO WARRANTY.  This program is free
software, and you are welcome to redistribute it under certain
conditions.  See <http://www.gnu.org/licenses/gpl.html>, for more
details.")

(define (help-message)
  "Usage: kise [OPTION]...
  -d, --debug       open a debugger aside the application
      --help        display this usage information
      --version     display version information")

(define (uninstalled-lang-message)
    "
  Warning:
    Your LANG environment variable is set to ~S which is not installed
    on this system. As a fallback, we will use ~S locale instead.\n")

(define (display-welcome port)
  ;; 'GNU Kise', really nice! But let's wait GNU evaluation/acceptance
  ;; (aglobs/display (string-append "GNU Kise " (aglobs/get 'version)) port)
  (aglobs/display (string-append "Kise " (aglobs/get 'version)) port)
  (aglobs/display (copyright-message) port))

(define (display-help port)
  (aglobs/display (help-message) port))


(eval-when (compile load eval)

  (define (display-uninstalled-lang port lang fallback)
    (aglobs/display (format #f "~?" (uninstalled-lang-message)
			    (list lang fallback)) port))

  (define (set-locale msg-port)
    (let ((locale (aglobs/get 'lang)))
      (catch #t
	(lambda () (setlocale LC_ALL "")) ;; gettext will not work otherwise
	(lambda (key . parameters)
	  ;; (format (current-error-port) "Uncaught throw to ’~a: ~a\n" key parameters)
	  (let* ((utf8-locales (sys/get-utf8-locales))
		 (fallback (or (sys/get-utf8-fallback '("C" "en"))
			       (and (not (null? utf8-locales)) (car utf8-locales))
			       "C")))
	    (display-uninstalled-lang msg-port locale fallback)
	    (setenv "LANG" fallback)
	    (set! locale fallback)
	    (setlocale LC_ALL fallback))
	  #f))
      (aglobs/set 'locale locale)
      locale))

  (aglobs/set 'command-synopsis
	      '((debug (single-char #\d) (value #f))
		(version (value #f))
		(help (value #f))))
  (aglobs/set 'version "0.9.4")
  (textdomain "main")
  (bindtextdomain "main" (aglobs/get 'pofdir))

  ;; these 2 must be executed before writting to the port AND loading
  ;; any gnome modules
  (let ((port (current-output-port)))
    (set-port-encoding! port "utf-8")
    (set-locale port)))


;;;
;;; 2d load modules step
;;;

(use-modules (system repl server)
	     (oop goops)
	     (gnome gnome)
	     (gnome gnome-ui) ;; <- uses $LANG
	     (gnome gtk)
	     (gnome glib)
	     (kise kise))

(default-duplicate-binding-handler
  '(merge-generics replace warn-override-core warn last))


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
    (cond (version? (display-welcome port))
	  (help? (display-help port))
	  (else
	   (display-welcome port)
	   (aglobs/set 'debug debug?)
	   (gtk-rc-parse (aglobs/get 'gtkrcfile))
	   (load-user-init)
	   (gnome-program-init "Kise" version)
	   (gnome-authentication-manager-init)
	   (kise/animate-ui uname (aglobs/get 'gladefile) version debug?)
	   (if debug? (spawn-server (make-tcp-server-socket #:port 1969)))
	   (g-main-loop-run (g-main-loop-new))))))


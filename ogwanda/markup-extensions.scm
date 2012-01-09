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

;; this code has been written as an experience to give users some
;; simple, yet powerful, markup commands [in a textview]. we keep it,
;; think it could be usefull for Kisê, maybe ... if later used, it
;; must be coupled with a 'commands.tex' file in latex path.

;;; Code:

(define-module (tex markup-extensions)
  ;; guile
  :use-module (ice-9 format)
  :use-module (ice-9 receive)
  :use-module (ice-9 regex)

  ;; common
  :use-module (macros do)
  :use-module (strings strings)
  :use-module (nbs all)

  :export (tex/tex-unite
	   tex/tex-unite-i-interne
	   tex/get-quantity-str
	   tex/get-quantity-for-numprint-str))


(define *tex/cc-oc-reserved-words*
  '("\\mimage"
    "\\mtext"
    "\\timage"
    "\\wimage"
    "\\blist"
    "\\clist"
    ;; "\\mlist"
    ;; "\\nlist"
    "\\crouge"
    "\\cvert"
    "\\cbleu"
    "\\corange"
    "\\gras"
    "\\italic"
    "\\souligne"
    ;; "\\textlambda"
    "\\tsup"
    "\\tsub"
    ))

(define (tex/reserved-words-lookup-1 str words results prep-tex?)
  (cond ((string-null? str)
	 (reverse! results))
	((null? words)
	 (reverse! (cons (list (prep-tex? str))
			 results)))
	(else
	 (let* ((word-start (string-contains str (car words)))
		(expr-start (and word-start
				 (string-index str #\{ word-start)))
		(expr-end   (and word-start
				 (string-index str #\} expr-start))))
	   ;; (format #t "Start @: ~A, Ends @: ~A~%" word-start expr-end)
	   (if word-start
	       (if expr-end
		   (tex/reserved-words-lookup-1 (substring str (1+ expr-end))
						words
						(cons (list (tex/reserved-words-lookup-1 (substring str 0 word-start)
											 (cdr words)
											 (list)
											 prep-tex?)
							    (string-append (substring str word-start (1+ expr-start))
									   (prep-tex? (substring str (1+ expr-start) expr-end))
									   "}"))
						      results)
						prep-tex?)
		   (begin
		     (format #t "
   !!! ERROR: !!!
	no ending for the reserved word ~A starting at ~A for the [sub]string ~S~%"
			     (car words) word-start str)
		     #f))
	       (tex/reserved-words-lookup-1 str (cdr words) results prep-tex?))))))

(define (tex/display-reserved-word-working-values expr-start rest-end before the-word rest content after)
  (format #t "
Bracket start ~A, Bracket [rest] end ~A: 
      Before: ~S
    the-word: ~S
        rest: ~S
     content: ~S
       after: ~S~%" 
	  expr-start rest-end before the-word rest content after))

(define (tex/display-reserved-word-formating-error-msg word starting-index str op-bracket-index rest rest-end)
  (format #t "
   !!! ERROR: !!!
	 reserved-word-lookup: ~S
   	           in: ~S
   open-bracket-index: ~A
	     the rest: ~S
  close-bracket-index: ~S~%"
	  word str op-bracket-index rest rest-end))

(define (tex/reserved-words-lookup-2 str words results prep-tex?)
  (cond ((string-null? str) (reverse! results))
	((null? words) (reverse! (cons (list (prep-tex? str)) results)))
	(else
	 (let* ((word-start (str/reserved-word-lookup str words))
		(op-bracket-index (and word-start
				       (string-index str #\{ word-start)))
		(rest (and op-bracket-index (substring str (1+ op-bracket-index))))
		(rest-end (and op-bracket-index (str/closing-curly-bracket-string-index rest))))
	   (if word-start
	       (if (and op-bracket-index rest-end)
		   (let ((before   (substring str 0 word-start))
			 (the-word (substring str word-start (1+ op-bracket-index)))
			 (content  (substring str
					      (+ op-bracket-index 1) 
					      (+ op-bracket-index 1 rest-end)))
			 (after    (substring str (+ 2 op-bracket-index rest-end))))
		     ;; (tex/display-reserved-word-working-values op-bracket-index rest-end before the-word rest content after)
		     (tex/reserved-words-lookup-2 after
						  words
						  (cons (list (prep-tex? before)
							      the-word
							      (list (tex/reserved-words-lookup-2 content words (list) prep-tex?)
								    "}"))
							results)
						  prep-tex?))
		   (begin
		     (tex/display-reserved-word-formating-error-msg (car words) word-start str op-bracket-index rest rest-end)
		     #f))
	       (tex/reserved-words-lookup-2 str (cdr words) results prep-tex?))))))

(define (tex/reserved-words-lookup str . prep-tex?)
  (if (null? prep-tex?)
      (tex/reserved-words-lookup-2 str *tex/cc-oc-reserved-words* (list) identity)
      (tex/reserved-words-lookup-2 str *tex/cc-oc-reserved-words* (list) (car prep-tex?))))

(define (tex/prep-cc-oc-str-for-tex-1 s-expr)
  (cond ((null? s-expr) "")
	((string? s-expr) s-expr)
	(else
	 (string-append (tex/prep-cc-oc-str-for-tex-1 (car s-expr))
			(tex/prep-cc-oc-str-for-tex-1 (cdr s-expr))))))

(define (tex/prep-cc-oc-str-for-tex str)
  ;; 1. reserved words string lookup
  ;; 2. running standard tex-prep on appropriate substrings
  ;; 3. reconstructing the string
  (let* ((s-expr (tex/reserved-words-lookup str tex/prep-str-for-tex))
	 (prep-str-1 (tex/prep-cc-oc-str-for-tex-1 s-expr))
	 (prep-str prep-str-1))
    (for-each (lambda (tex/str-tex-replace-item)
		(set! prep-str (str/replace-all prep-str
						(car tex/str-tex-replace-item)
						(cdr tex/str-tex-replace-item))))
	(append (tex/str-common-encoding-to-tex-replace-table)
		(tex/str-latin9-to-tex-replace-table)))
    prep-str))

(define (tex/str-common-encoding-to-tex-replace-table)
  ;; careful, the order is important
  (list (cons "m²" "m$^{2}$")
	(cons "m³" "m$^{3}$")
	(cons "±"  "$\\pm$")
	;; (cons "°C" "\\celsius")
	(cons "°C" "\\ensuremath{^\\circ}C")
	(cons "°" "\\ensuremath{^\\circ}")
	(cons "§" "\\textsection")
	))

(define (tex/str-latin1-to-tex-replace-table)
  (list (cons "»" "$\\lambda$")))

(define (tex/str-latin9-to-tex-replace-table)
  ;; attention, l'ordre peut avoir de l'importance
  (list (cons "¾" "\\textlambda")
	(cons "¤" "\\euro")))

(define (tex/tex-unite unity)
  (if unity
      (let ((unite (string-downcase unity)))
	(case (string->symbol unite)
	  ((m m1)     "m")
	  ((m2)       "m$^{2}$")
	  ((m3)       "m$^{3}$")
	  ((ma)       "stair(s)")
	  ((ff pi pc) "~~") ;; \\einsec"
	  (else
	   unite)))
      "~~"
      ;; \\einsec"
      ))

(define (tex/tex-unite-i-interne unity quantity)
  (if unity
      (let ((unite (string-downcase unity)))
	(case (string->symbol unite)
	  ((m m1)     "m")
	  ((m2)       "m$^{2}$")
	  ((m3)       "m$^{3}$")
	  ((ff)       "package")
	  ((pi pc) 
	   (if (> quantity 1)
	       "pieces"
	       "piece"))
	  ((ma)       
	   (if (> quantity 1)
	       "stairs"
	       "stair"))
	  (else
	   unite)))
      "\\einsec"))

(define (tex/get-quantity-str quantity unity)
  ;; la difference avec ociter/get-quantity-str c'est
  ;; qu'ici ma [marches] doit donner un entier
  (let* ((unit-symbol    (if unity (string->symbol (string-downcase unity)) -1))
	 (decimal-number (case unit-symbol
			   ((m m1) 1)
			   ((m2) 2)
			   ((m3) 3)
			   ((ff) (if (integer? quantity) 0 2))
			   (else
			    0)))
	 (q-str (case decimal-number
		  ((0) (nb/commify-1 quantity 0 "    "))
		  ((1) (nb/commify-1 quantity 1 "  "))
		  ((2) (nb/commify-1 quantity 2 " "))
		  ((3) (nb/commify-1 quantity 3)))))
    (values q-str
	    ;; alerte si entier et la valeur ne l'est pas
	    (and (= decimal-number 0)
		 (not (fp/=? quantity (round quantity)))))))


(define (tex/get-quantity-for-numprint-str quantity unity)
  ;; here, 'ma' [steps] must return an integer
  (let* ((unit-symbol (if unity (string->symbol (string-downcase unity)) -1))
	 (decimal-number (case unit-symbol
			   ((m m1) 1)
			   ((m2) 2)
			   ((m3) 3)
			   ((ff) (if (integer? quantity) 0 2))
			   (else
			    0)))
	 (q-str (case decimal-number
		  ((0) (nb/numprintify quantity 0 "    "))
		  ((1) (nb/numprintify quantity 1 "  "))
		  ((2) (nb/numprintify quantity 2 " "))
		  ((3) (nb/numprintify quantity 3)))))
    (values q-str
	    ;; alerte si entier et la valeur ne l'est pas
	    (and (= decimal-number 0)
		 (not (fp/=? quantity (round quantity)))))))


#!

(use-modules (tex markup-extensions))
(reload-module (resolve-module '(tex markup-extensions)))


!#

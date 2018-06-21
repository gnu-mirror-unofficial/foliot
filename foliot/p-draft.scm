;; -*- mode: scheme; coding: utf-8 -*-

;;;;
;;;; Copyright (C) 2011 - 2018
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


(define-module (foliot p-draft)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-17)
  #:use-module (ice-9 receive)
  #:use-module (ice-9 format)
  #:use-module (oop goops)
  #:use-module (gnome gobject)
  #:use-module (gnome gtk)
  #:use-module (grip iter)
  #:use-module (grip date)
  #:use-module (grip passwd)
  #:use-module (grip i18n)
  #:use-module (grip number)
  #:use-module (grip latex)
  #:use-module (grip gnome)
  #:use-module (foliot globals)
  #:use-module (foliot db)
  #:use-module (foliot iter)
  #:use-module (foliot tl-widget)
  #:use-module (foliot p-dialog)
  #:use-module (foliot p-common)

  #:duplicates (merge-generics
		replace
		warn-override-core
		warn
		last)

  #:export (fp/print-draft))


(eval-when (expand load eval)
  (textdomain "p-draft")
  (bindtextdomain "p-draft" (ref %foliot-store 'pofdir)))


;;;
;;; Globals
;;;


;;;
;;; The draft.tex file itself
;;;

(define (fp/write-draft-class ostream)
  (tex/write-utf8-comment-line ostream)
  (format ostream "
\\documentclass[
  a4paper,
  9pt]
{extarticle}~%"))

(define (fp/write-draft-inputs ostream)
  (let ((latexdir (ref %foliot-store 'latexdir)))
    (format ostream "
  \\input{~A/draft-packages}
  \\input{~A/draft-commandes}
  \\input{~A/colordefs}
  \\input{~A/global-variables}~%"
	    latexdir latexdir latexdir latexdir)))

(define (fp/write-draft-lvars-input ostream pdir lvars-fname)
  (format ostream "
  \\input{~A/~A}~%" pdir lvars-fname))

(define (fp/write-draft-fancy-specs ostream)
  (format ostream "
	\\lhead{\\flheader}
	\\chead{}
	\\rhead{\\frheader}

	\\lfoot{\\flfooter}
	\\cfoot{}
	\\rfoot{\\frfooter}

	\\renewcommand{\\headrulewidth}{0pt}
	\\renewcommand{\\footrulewidth}{0.3pt}~%"))

(define (fp/write-draft-begin-document ostream)
  (format ostream "
\\begin{document}
  \\pagestyle{fancy}
  %% \\tableofcontents

"))

(define (fp/write-nptsepdsig ostream . tsepdsig)
  ;; default is english but we should get it from locale
  (format ostream "
  \\npthousandsep{~A}
  \\npdecimalsign{~A}~%~%"
	  (if (null? tsepdsig) "," (caar tsepdsig))
	  (if (null? tsepdsig) "." (cdar tsepdsig))
	  ))

(define (fp/write-logo-spec-code ostream ulogo)
  (if ulogo
      (format ostream "
  \\newlength\\logowidth
  \\newlength\\logoheight

  \\setlength{\\logowidth}{\\widthof{\\flheaderlogo}}
  \\setlength{\\logoheight}{\\totalheightof{\\flheaderlogo}}

  \\ifdim\\logoheight<42pt
    \\newlength\\diffheight
    \\setlength{\\diffheight}{42pt-\\logoheight}
    \\def\\flheader{\\flheaderlogo \\\\[\\diffheight]}
  \\else
    \\def\\flheader{\\flheaderlogo}
  \\fi

  %logo width: \\the\\logowidth\\\\
  %logo height: \\the\\logoheight\\\\
")))

#;(define (fp/write-logo-spec-code ostream ulogo)
  (if ulogo
      (format ostream "
    \\def\\flheader{\\flheaderlogo}
")))

(define (fp/write-draft-end-document ostream)
  (format ostream "
\\end{document}
"))

(define (fp/write-draft-abstract-table-rows-1 ostream db-name where group-by order-by)
  (let ((row 1))
    (for-each (lambda (row-spec)
		(format ostream "
      \\rowcolor{~A}
        \\flabel{~A} : & ~A \\\\~%"
			(if (odd? row) "\\ftboddrowcolour" "\\ftbevenrowcolour")
			(car row-spec)
			(cdr row-spec))
		(set! row (1+ row)))
	(list (cons (_ "database")
		    (format #f "\\textcolor{\\fdbnamecolour}{~A}" db-name))
	      (cons (_ "filter")
		    (format #f "~A" (if where (tex/prep-str-for-tex where) (_ "none"))))
	      (cons (_ "grouped by")
		    (format #f "~A" (if group-by (tex/prep-str-for-tex group-by) (_ "none"))))
	      (cons (_ "ordered by")
		    (format #f "~A" (if order-by (tex/prep-str-for-tex order-by) (_ "none"))))
	      ))))

(define (fp/write-draft-abstract-table-hline ostream)
  (format ostream "
      \\\\[-3mm] \\hline \\\\[-2.5mm]~%"))

(define (fp/write-draft-abstract-table-rows-2 ostream tl-widget db-name where group-by order-by row)
  (receive (ttime tdays ctime cdays)
      (ftlw/get-totals tl-widget)
    (for-each (lambda (row-spec)
		(format ostream "
      \\rowcolor{~A}
        \\flabel{~A} : & ~A \\\\~%"
			(if (odd? row) "\\ftboddrowcolour" "\\ftbevenrowcolour")
			(car row-spec)
			(cdr row-spec))
		(set! row (1+ row)))
	(list (cons (_ "total time")
		    (format #f "~A ~A - ~A ~A"
			    ttime (ftlw/get-hour-hours ttime)
			    tdays (ftlw/get-day-days tdays)))
	      (cons (_ "charged time")
		    (format #f "~A ~A - ~A ~A"
			    ctime (ftlw/get-hour-hours ctime)
			    cdays (ftlw/get-day-days cdays)))
	      ))))

(define (fp/write-draft-abstract-table ostream tl-widget db-name where group-by order-by)
  (format ostream "
\\setlength\\arrayrulewidth{0.3pt}\\arrayrulecolor{\\fabstitlecolour}~%")
  (format ostream "
 \\begin{center}
   \\begin{tabularx}{.94\\textwidth}{r X}~%")
  (fp/write-draft-abstract-table-rows-1 ostream db-name where group-by order-by)
  (fp/write-draft-abstract-table-hline ostream)
  (fp/write-draft-abstract-table-rows-2 ostream tl-widget db-name where group-by order-by 5)
  (format ostream "
   \\end{tabularx}\\\\[1mm]
 \\end{center}~%~%"))

(define (fp/get-abstract-footnote-text)
  (format #f"~A."
	  (string-append (_ "When selected [not grouped by]")
			 ", \\flabel{" (_ "to be charged") "} "
			 (_ "is printed using")
			 " \\checked" "~"
			 (_ "when true") " "
			 (_ "and") " " "\\textlnot" "~"
			 (_ "when false"))))

(define (fp/get-core-fields-text core-fields)
  (let ((result ""))
    (for-each (lambda (field)
		(let ((field-name (tex/prep-str-for-tex (symbol->string (car field)))))
		  (set! result
			(if (string-null? result)
			    (format #f "\\flabel{~A}" field-name)
			    (format #f "~A, \\flabel{~A}" result field-name)))))
	core-fields)
    result))

(define (fp/write-draft-abstract-used-columns ostream core-fields)
  (format ostream "~A\\footnote{~A}: ~A.~%"
	  (_ "In the core of this report, printed GNU Foliot's fields are")
	  (fp/get-abstract-footnote-text)
	  (fp/get-core-fields-text core-fields)))

(define (fp/write-draft-abstract ostream tl-widget db-name where group-by order-by core-fields)
  (format ostream "
\\fabstract{~A}{" (_ "Summary"))
  (format ostream "~A:\\\\[-1mm]~%"
	  (_ "This \\foliot report was produced using the following criteria"))
  (fp/write-draft-abstract-table ostream tl-widget db-name where group-by order-by)
  (fp/write-draft-abstract-used-columns ostream core-fields)
  (format ostream "}~%~%"))

(define (fp/display-template-and-group-infos tpl-name tpl-grouping grouped order filter)
  (format #t "Draft content:
  template name: ~A
    group infos: ~S
        grouped: ~S
          order: ~S
         filter: ~S~%" tpl-name tpl-grouping grouped order filter))

(define (fp/display-this-list-of-consed-items items)
  (for-each (lambda (item)
	      (format #t "  ~A: ~A~%" (car item) (cdr item)))
      items))

(define (fp/get-grouped-and-order-items tpl-grouping)
  ;; this supposes that when an item is grouped it also has an
  ;; ascending or descending order spec [and never none]
  (let ((grouped (list))
	(ordered (list)))
    (for-each (lambda (item)
		(let ((field (db-pt/df-get 'name item))
		      (order (db-pt/df-get 'sort item)))
		  (if (db-pt/df-get 'group item)
		      (begin
			(set! grouped (cons item grouped))
			(set! ordered (cons item ordered)))
		      (case order
			((asc desc) (set! ordered (cons item ordered)))
			(else 'nothing)))))
	tpl-grouping)
    (values (reverse! grouped) (reverse! ordered))))

(define (fp/order-by-extractor item)
  ;; item is like (#f "who" none)like db-pt/default-field
  (let* ((i-name (db-pt/df-get 'name item))
	 (fieldname (if (string=? i-name "what") "what" i-name))
	 (order (db-pt/df-get 'sort item)))
    (case order
      ((none) fieldname)
      ((asc) (string-append fieldname " asc"))
      ((desc) (string-append fieldname " desc")))))

(define (fp/get-groups group-by)
  (let ((levels (list)))
    (for-each (lambda (item)
		(let* ((field (db-pt/df-get 'name item))
		       (its-symb (string->symbol field)))
		  (set! levels
			(cons (cons its-symb (db-foliot/get-pos its-symb)) levels))))
	group-by)
    (reverse! levels)))

(define (fp/fill-current current group-values)
  (dotimes (i (length group-values))
    (vector-set! current i (list-ref group-values i))))

(define (fp/get-group-values groups tuple)
  (let ((values (list)))
    (dotimes (i (length groups))
      (set! values
	    (cons (db-foliot/get tuple (car (list-ref groups i)))
		  values)))
    (reverse! values)))

(define (fp/group-compare-current current tuple-group-values)
  ;; number of #t, pos of the first #f
  ;; (format #t "fp/group-compare-current: ~S, ~S~%" current  tuple-group-values)
  (let ((c-values (list))
	(same-nb 0)
	(different-pos -1))
    (dotimes (i (length tuple-group-values))
      (let* ((c-val (list-ref tuple-group-values i))
	     (same? (cond ((string? c-val) (string=? (vector-ref current i) c-val))
			  ((number? c-val) (float=? (vector-ref current i)
                                                    (* c-val 1.0)))         ;; ensure float
			  (else
			   (format #t "Warning: fp/group-compare-current, unmanaged value type ~S~%" c-val)
			   #t)))) ;; <- not knowing, we suppose it is the same as before
	(if same?
	    (set! same-nb (1+ same-nb))
	    (when (= different-pos -1) (set! different-pos i)))
	(set! c-values (cons c-val c-values))))
    (set! c-values (reverse c-values))
    (values c-values same-nb different-pos)))

(define (fp/write-draft-section-title ostream group level tuple)
  (let* ((accessor (car group))
	 (value (db-foliot/get tuple (car group)))
	 (title (if (eq? accessor 'to_be_charged)
		    (case (string->symbol value)
		      ((f) "not charged")
		      ((t) "to be charged"))
		    value)))
    (case level
      ((0) (format ostream "\\fsection{~A}~%" title))
      ((1) (format ostream "\\fsubsection{~A}~%" title))
      ((2) (format ostream "\\fsubsubsection{~A}~%" title))
      ((3) (format ostream "\\fparagraph{~A}~%" title)))))

(define (fp/get-core-table-offset groups)
  (case (length groups)
    ((0) "0cm")
    ((1) ".2cm")
    ((2) ".54cm")
    ((3) "1.2cm")
    ((4) "1.7cm")))

;; when 2 rows [description on the second], an extra field is added
;; and row 1 field 1 uses a multicolumn, for better readability of the
;; row's description and of the table in general.
(define (fp/get-ltx-preamble-rowfmt-nb-cols core-fields)
  (let* ((description? (member 'description core-fields (lambda (x y) (eq? x (car y)))))
	 (preamble "")
	 (rowfmt "")
	 (nb-cols 0))
    (for-each (lambda (field)
		(let ((f-name (car field))
		      (f-tex-spec (caddr field)))
		  (if (string-null? preamble)
		      (if description?
			  (begin
			    (set! nb-cols (+ nb-cols 2))
			    (set! preamble (string-append rowfmt "l " f-tex-spec))
			    (case f-name
			      ((date_)
			       (set! rowfmt "\\multicolumn{2}{c}{\\cellcolor{\\ftboddrowcolour}~A}"))
			      (else
			       (set! rowfmt "\\multicolumn{2}{l}{\\cellcolor{\\ftboddrowcolour}~A}"))))
			  (begin
			    (set! nb-cols (1+ nb-cols))
			    (set! preamble f-tex-spec)
			    (set! rowfmt (string-append rowfmt "~A"))))
		      (begin
			(set! nb-cols (1+ nb-cols))
			(set! preamble (string-append preamble " " f-tex-spec))
			(set! rowfmt (string-append rowfmt " & ~A"))))))
	core-fields)
    (values preamble rowfmt nb-cols description?)))


#!
\rowcolors{1}{green}{pink}

The command \hiderowcolors is available to deactivate highlighting
from a specified row until the end of the table. Highlighting can be
reactivated within the table via the \showrowcolors command.
!#


(define (fp/write-draft-begin-ltx ltx-stream preamble)
  (format ltx-stream "
\\begin{longtable}{~A}~%"
	  preamble))

(define (fp/write-draft-end-ltx ltx-stream)
  (format ltx-stream "
\\end{longtable}~%"))

(define (fp/get-ltx-first-row-values core-fields tuple)
  (let ((row-values (list)) ;; a list of strings
	(j 0)) ;; the k to pass [maybe] to tex/prep
    (for-each (lambda (core-field)
		(let* ((field-name (car core-field))
		       (val (db-foliot/get tuple field-name))
		       (tex/prep? (list-ref core-field 3)))
		  (set! row-values
			(cons (case field-name
				((description) "") ;; now a second row in ltx
				((to_be_charged)
				 (if (string=? val "t") "\\checked" "\\textlnot"))
				(else
				 (if tex/prep? (tex/prep-str-for-tbx-env val j #t) val)))
			      row-values))
		  (set! j (1+ j))))
	core-fields)
    (reverse! row-values)))

(define (fp/get-ltx-description tuple)
  (tex/prep-str-for-tbx-env (db-foliot/get tuple 'description) 0 #t)) ;; newline -> space

(define (fp/draft-ltx-second-row-fmt even-bg?)
  (if even-bg?
      "  & \\multicolumn{~A}{p{\\textwidth-~A-4\\tabcolsep}}{\\cellcolor{\\ftbevenrowcolour}\\small ~A}\\\\~%"
      "  \\multicolumn{1}{l}{\\cellcolor{white}~~} & \\multicolumn{~A}{p{\\textwidth-~A-4\\tabcolsep}}{\\small ~A}\\\\~%"))

(define (fp/write-draft-ltx-tuple ltx-stream tuple core-fields rowfmt nb-cols description? ltx-offset)
  ;; (format #t "~S~%" rowfmt)
  (let ((1st-row-values (fp/get-ltx-first-row-values core-fields tuple)))
    ;; (format #t "~S~%" 1st-row-values)
    ;; (format ltx-stream "  \\rowcolor{\\ftboddrowcolour}~%")
    (format ltx-stream "  ~?\\\\~%" rowfmt 1st-row-values)
    (when description?
      ;; (format ltx-stream "  \\rowcolor{\\ftbevenrowcolour}~%")
      (format ltx-stream "~?" (fp/draft-ltx-second-row-fmt 'even-bg)
	      (list (1- nb-cols) ltx-offset (fp/get-ltx-description tuple))))))

(define (fp/write-draft-ltx-tuples ltx-stream tuples core-fields rowfmt nb-cols description? ltx-offset)
  (for-each (lambda (tuple)
	      (fp/write-draft-ltx-tuple ltx-stream tuple core-fields rowfmt nb-cols description? ltx-offset))
      tuples))

(define (fp/write-draft-core-sections ostream groups level tuple)
  ;; level is from 0 to n-1, the starting group from which to print
  ;; the corresponding section,subsections, ... levels, then comes the tuple
  (let ((eff-groups (case level
		      ((0) groups)
		      ((1) (cdr groups))
		      ((2) (cddr groups))
		      ((3) (cdddr groups))))
	(j level))
    (for-each (lambda (eff-group)
		(fp/write-draft-section-title ostream eff-group j tuple)
		(set! j (1+ j)))
	eff-groups)))

(define (fp/write-ltx-offset ostream ltx-offset)
  (format ostream "
  \\setlength\\LTleft{~A}
  \\setlength\\LTright{0pt}
  \\rowcolors{1}{\\ftboddrowcolour}{\\ftbevenrowcolour}
  ~%"
	  ltx-offset))

(define (fp/write-draft-ltxtable-cmd ostream ltx-shortname)
  (format ostream "  \\LTXtable{\\linewidth}{~A}~%" ltx-shortname))

#!
;; this is now catched earlier
(if (and (null? groups)
	 (null? core-fields))
    (md1b/select-gui (dialog tl-widget)
		     (_ "Warning")
		     (_ "No grouping no printing:")
		     (_ "There is nothing to print. You must select at least one field to print and/or group.")
		     (lambda () 'nothing)
		     'dialog-warning))
!#

(define (fp/display-ltx-preamble-debug-info groups core-fields preamble rowfmt nb-cols description?)
  (format #t "
     Groups: ~S
Core fields: ~S
   Preamble: ~S
 Row format: ~S~%~%"
	  groups core-fields preamble rowfmt))

(define (fp/write-draft-core-content ostream tuples groups
                                     core-fields fp-widget tl-widget draftLT-file-object)
  (let* ((current (make-vector (length groups) #f))
	 (pdir (p-directory draftLT-file-object))
	 (ltx-offset (fp/get-core-table-offset groups))
	 (ltxtable-idx 0)
	 (ltx-shortname (format #f "~A~A" (short-filename draftLT-file-object) ltxtable-idx))
	 (ltx-fullname (format #f "~A/~A.tex" pdir ltx-shortname))
	 (ltx-stream (open-output-file ltx-fullname)))
    (fp/write-ltx-offset ostream ltx-offset)
    (receive (preamble rowfmt nb-cols description?)
	(fp/get-ltx-preamble-rowfmt-nb-cols core-fields)
      (if (ref %foliot-store 'debug)
          (fp/display-ltx-preamble-debug-info groups core-fields
                                              preamble rowfmt nb-cols description?))
      (if (null? groups)
	  (begin
	    (format ostream "\\bigskip~%")
	    (fp/write-draft-ltxtable-cmd ostream ltx-shortname)
	    (tex/write-utf8-comment-line ltx-stream)
	    (fp/write-draft-begin-ltx ltx-stream preamble)
	    (fp/write-draft-ltx-tuples ltx-stream tuples
                                       core-fields rowfmt nb-cols description? ltx-offset)
	    (fp/write-draft-end-ltx ltx-stream)
	    (close ltx-stream))
	  (begin
	    (for-each (lambda (tuple)
			(let ((group-values (fp/get-group-values groups tuple)))
			  (if (not (vector-ref current 0)) ;; <- firstp?
			      (begin
				(fp/fill-current current group-values)
				(fp/write-draft-core-sections ostream groups 0 tuple)
				(unless (null? core-fields)
				  (fp/write-draft-ltxtable-cmd ostream ltx-shortname)
				  (tex/write-utf8-comment-line ltx-stream)
				  (fp/write-draft-begin-ltx ltx-stream preamble)
				  (fp/write-draft-ltx-tuple ltx-stream tuple core-fields rowfmt nb-cols description? ltx-offset)))
			      (receive (c-values same-nb different-pos)
				  (fp/group-compare-current current group-values)
				;; (format #t "comp cur.:~%  c-values: ~S, same-nb: ~A, different-pos ~A~%"
				;; c-values same-nb different-pos)
				(case different-pos
				  ((-1)
				   (unless (null? core-fields)
				     (fp/write-draft-ltx-tuple ltx-stream tuple core-fields rowfmt nb-cols description? ltx-offset)))
				  (else
				   (fp/fill-current current group-values)
				   (unless (null? core-fields)
				     (fp/write-draft-end-ltx ltx-stream)
				     (close ltx-stream)
				     (set! ltxtable-idx (1+ ltxtable-idx))
				     (set! ltx-shortname (format #f "~A~A" (short-filename draftLT-file-object) ltxtable-idx))
				     (set! ltx-fullname (format #f "~A/~A.tex" pdir ltx-shortname))
				     (set! ltx-stream (open-output-file ltx-fullname)))
				   (fp/write-draft-core-sections ostream groups different-pos tuple)
				   (unless (null? core-fields)
				     (fp/write-draft-ltxtable-cmd ostream ltx-shortname)
				     (tex/write-utf8-comment-line ltx-stream)
				     (fp/write-draft-begin-ltx ltx-stream preamble)
				     (fp/write-draft-ltx-tuple ltx-stream tuple core-fields rowfmt nb-cols description? ltx-offset))))))))
		;; (list (car tuples) (cadr tuples) (caddr tuples))
		tuples)
	    (unless (null? core-fields)
	      (fp/write-draft-end-ltx ltx-stream)
	      (close ltx-stream)))))))

(define (fp/write-draft-content ostream fp-widget tl-widget draftLT-file-object)
  (let* ((db-name (basename (db-file tl-widget)))
	 (filter (active-filter tl-widget))
	 (ulogo (fcfg/get 'ulogo))
	 (tpl-pos (get-active (template-combo fp-widget)))
	 (tpl-tuple (db-pt/get-tuple (tpl-tuples fp-widget) tpl-pos))
	 (tpl-name (db-pt/get tpl-tuple 'name))
	 (tpl-items (db-pt/get tpl-tuple 'items))
	 (tpl-mode (db-pt/get tpl-tuple 'mode))
	 (tpl-grouping-str (db-pt/get tpl-tuple 'group_and_sort))
	 (tpl-grouping (with-input-from-string tpl-grouping-str read)))
    (receive (grouped order)
	(fp/get-grouped-and-order-items tpl-grouping)
      ;; (fp/display-template-and-group-infos tpl-name tpl-grouping grouped order filter)
      (let* ((where (active-filter tl-widget))
	     (group-by (sqlite/build-group-by-order-by-expression grouped (lambda (item) (db-pt/df-get 'name item))))
	     (groups (fp/get-groups grouped))
	     (core-fields (fp/get-core-ltx-field-specs fp-widget))
	     (order-by (sqlite/build-group-by-order-by-expression order fp/order-by-extractor))
	     (tuples (db-foliot/select-another-some where
						  #f ;; group-by: we do it, otherwise we wouldn't get all tuples
						  order-by)))
	;; (fp/display-this-list-of-consed-items
	;;   (list (cons "where" where) (cons "group by" group-by) (cons "order by" order-by)))
	;; (format #t "core-fields: ~S~%" core-fields)
	(fp/write-nptsepdsig ostream '("," . "."))
	(fp/write-logo-spec-code ostream ulogo)
	(fp/write-draft-abstract ostream tl-widget db-name where group-by order-by core-fields)
	(fp/write-draft-core-content ostream tuples groups core-fields fp-widget tl-widget draftLT-file-object)))))

(define (fp/write-draft-tex-file fp-widget tl-widget tex-files)
  (let* ((lvars-file-object (lvars tex-files))
	 (lvars-shortname (short-filename lvars-file-object))
	 (pdir (p-directory lvars-file-object))
	 (draftLT-file-object (draftLT tex-files))
	 ;; (draftLT-shortname (short-filename draftLT-file-object))
	 (draft-file-object (draft tex-files))
	 (ostream (open-output-file (full-filename draft-file-object))))
    (fp/write-draft-class ostream)
    (fp/write-draft-inputs ostream)
    (fp/write-draft-lvars-input ostream pdir lvars-shortname)
    (fp/write-draft-fancy-specs ostream)
    (fp/write-draft-begin-document ostream)
    (fp/write-draft-content ostream fp-widget tl-widget draftLT-file-object)
    (fp/write-draft-end-document ostream)
    (close ostream)))


;;;
;;; API
;;;

(define (fp/print-draft fp-widget tl-widget tex-files)
  (fp/write-draft-tex-file fp-widget tl-widget tex-files)
  (fp/compile-tex-file (draft tex-files))
  ;; (fp/write-draftLT-tex-file fp-widget tl-widget tex-files)
  (if (pdf tex-files)
      (fp/write-pdf tex-files 'draft)
      (fp/write-printer tex-files 'draft
			;; (gp/get-printer-cups-name (get-active (imprimante-combo ocp/widget)))
			'the-printer
			)))


#!

(use-modules (foliot p-draft))
(reload-module (resolve-module '(foliot p-draft)))

this bugs latex: [tex/prep] just to test

http://en.wikibooks.org/w/index.php?title=LaTeX/Tables&stable=0#Need_more_complicated_features.3F

;;;
;;; Prev [old] code
;;;

(define (fp/write-draft-content ostream draftLT-shortname)
  (format ostream "
\\begin{document}
  \\pagestyle{fancy}
  %% \\tableofcontents

  {\\LTXtable{\\linewidth}{~A}}

\\end{document}"
	  draftLT-shortname))

(define (fp/get-core-table-tex-def core-fields groups)
  (receive (preamble rowfmt)
      (fp/get-core-table-preamble core-fields)
    (let* ((nb-groups (length groups))
	   (offset (and (> nb-groups 0)
			(fp/get-core-table-offset groups))))
      (case nb-groups
	((0) (format #f "
\\begin{center}
  \\begin{tabularx}{\\textwidth}{~A}
    ~A
  \\end{tabularx}
\\end{center}~%"
		     preamble rowfmt))
	(else (format #f "
{\\hskip ~A
  \\begin{tabularx}{\\textwidth-~A}{~A}
    ~A
  \\end{tabularx}}~%"
	      offset offset preamble rowfmt))))))

!#

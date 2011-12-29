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

;; '*-draftLT.tex' content

;;; Code:

;; this code is not used and is incomplete [does not even define its
;; module], but we keep it as an example and 'just in case'.

(define  (kp/write-draftLT-header ostream)
  (format ostream "% -*- coding: utf-8; -*-

%%%
%%% Notes:
%%%
%%% @{}:
%%%   to suppress inter column spaces. in this document it is
%%%   not [yet] necessary;
%%% n{x}{y}:
%%%   x,y specify the number of digits before, after the decimal sign
%%%

\\npthousandsep{.}~%"))

(define (kp/write-draftLT-toprule ostream)
  (format ostream "

\\toprule~%"))

(define (kp/write-draftLT-midrule ostream)
  (format ostream "

\\midrule~%"))

(define (kp/write-draftLT-longtable-end ostream)
  (format ostream "

\\end{longtable}~%"))

(define (kp/write-draftLT-longtable-header ostream)
  (format ostream "
\\begin{longtable}{l l l l n{2}{1} c X }
  \\rowcolor{fbg-grey}
    \\einsec 		                        &               %2
    \\einsec 		                        &               %3
    \\einsec 	                        	&               %4
    \\einsec 	                        	&               %5
    \\einsec 	                        	&               %6
    \\multicolumn{1}{c}{\\cellcolor{fbg-grey}\\textbf{~A}} &    %7
    \\einsec \\\\                                               %8~%"
	  (_ "To be"))
  (format ostream "
  \\rowcolor{fbg-grey} 
    \\multicolumn{1}{c}{\\cellcolor{fbg-grey}\\textbf{~A}} &    %2
    \\multicolumn{1}{c}{\\cellcolor{fbg-grey}\\textbf{~A}} &    %3
    \\multicolumn{1}{c}{\\cellcolor{fbg-grey}\\textbf{~A}} &    %4
    \\multicolumn{1}{c}{\\cellcolor{fbg-grey}\\textbf{~A}} &    %5
    \\multicolumn{1}{c}{\\cellcolor{fbg-grey}\\textbf{~A}} &    %6
    \\multicolumn{1}{c}{\\cellcolor{fbg-grey}\\textbf{~A}} &    %7
    \\multicolumn{1}{c}{\\cellcolor{fbg-grey}\\textbf{~A}} \\\\ %8
    %% \\hline
\\endhead~%"
	  (_ "Date")
	  (_ "Who")
	  (_ "for whom")
	  (_ "What")
	  (_ "Hours")
	  (_ "charged")
	  (_ "Description")))

(define (kp/write-draftLT-longtable-content ostream kp-widget tl-widget)
  (format ostream "
12.06.2011 &
   \\textcolor{DarkGreen}{me} &
   \\textcolor{DarkGreen}{the client} &
   \\textcolor{DarkGreen}{/admin/email/tp5e-3} &
   {\\color{DarkGreen}} 5.3 &
   \\textcolor{DarkGreen}{yes} &
   \\textcolor{DarkGreen}{a long email to explain all we did that is charged} \\\\

\\einsec &
   \\textcolor{MyDarkBlue}{you} &
   \\textcolor{MyDarkBlue}{the client} &
   \\textcolor{MyDarkBlue}{/admin/email/tp5e-3} &
   {\\color{MyDarkBlue}} 4.2 &
   \\textcolor{MyDarkBlue}{yes} &
   \\textcolor{MyDarkBlue}{what did you do on this project?} \\\\

\\midrule

13.06.2011 &
   me &
   another client &
   /sysadmin/install &
   3.8 &
   yes &
   debian testing full config\\\\

\\midrule

14.06.2011 &
   you &
   the client &
   /sysadmin/update &
   0.7 &
   yes &
   network tools and config \\\\"))


(define (kp/write-draftLT-tex-file kp-widget tl-widget tex-files)
  (let* ((draftLT-file-object (draftLT tex-files))
	 (ostream (open-output-file (full-filename draftLT-file-object))))
    (kp/write-draftLT-header ostream)
    (kp/write-draftLT-longtable-header ostream)
    (kp/write-draftLT-toprule ostream)
    (kp/write-draftLT-longtable-content ostream kp-widget tl-widget)
    (kp/write-draftLT-longtable-end ostream)
    (close ostream)))

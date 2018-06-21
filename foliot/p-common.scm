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


(define-module (foliot p-common)
  #:use-module (ice-9 format)
  #:use-module (oop goops)
  #:use-module (grip module)
  #:use-module (grip passwd)
  #:use-module (foliot globals)

  #:duplicates (merge-generics
		replace
		warn-override-core
		warn
		last)

  #:export (<tex-file>
	    <tex-files>
	    fp/common-filenames
	    fp/get-ps-fname-from-pdf
	    fp/compile-tex-file
	    fp/write-pdf
	    fp/write-printer))


(g-export show-me
          p-directory
          short-filename
          tex-filename
          ps-filename
          pdf-filename
          full-filename

          pdf
          ps
          doc-ref
          lvars
          draft
          draftLT
          commercial)


(eval-when (expand load eval)
  (re-export-public-interface (oop goops)
			      (grip passwd)
			      (foliot globals))
  (textdomain "p-common")
  (bindtextdomain "p-common" (ref %foliot-store 'pofdir)))


;;;
;;; GOOPS related stuffs
;;;

(define-class <tex-file> ()
  ;; Class allocated slots
  (p-directory    #:accessor p-directory
		  #:init-keyword #:p-directory
		  #:allocation #:class)
  ;; Instance allocated slots
  (short-filename #:accessor short-filename #:init-keyword #:short-filename #:init-value #f)
  (tex-filename   #:accessor tex-filename   #:init-keyword #:tex-filename   #:init-value #f)
  (ps-filename    #:accessor ps-filename    #:init-keyword #:ps-filename    #:init-value #f)
  (pdf-filename   #:accessor pdf-filename   #:init-keyword #:pdf-filename   #:init-value #f)
  (full-filename  #:accessor full-filename  #:init-keyword #:full-filename  #:init-value #f))

(define-method (show-me (tex-file <tex-file>))
  (for-each (lambda (slot)
	      (let ((name (slot-definition-name slot))
		    (acc  (slot-definition-accessor slot)))
		(when (slot-bound? tex-file name)
		  (format #t "~20@A: ~A~%" name (acc tex-file)))))
      (class-slots <tex-file>))
  (values))

(define-class <tex-files> ()
  ;; Class allocated slots
  (pdf     #:accessor pdf     
	   #:init-keyword #:pdf 
	   #:allocation #:class)
  (ps      #:accessor ps
	   #:init-keyword #:ps
	   #:allocation #:class)
  (doc-ref #:accessor doc-ref
	   #:init-keyword #:doc-ref
	   #:allocation #:class)
  ;; Instance allocated slots
  (lvars #:accessor lvars #:init-keyword #:lvars)
  (draft #:accessor draft #:init-keyword #:draft)
  (draftLT #:accessor draftLT #:init-keyword #:draftLT)
  (commercial #:accessor commercial #:init-keyword #:commercial))

(define-method (show-me (tex-files <tex-files>))
  (newline)
  (for-each (lambda (slot)
	      (let ((name (slot-definition-name slot))
		    (acc  (slot-definition-accessor slot)))
		(when (slot-bound? tex-files name)
		  (if (eq? (slot-definition-allocation slot) #:class)
		      (format #t "~10@A: ~A~%" name (acc tex-files))
		      (begin
			(simple-format #t "~A:~%" name)
			(show-me (acc tex-files)))))))
      (class-slots <tex-files>))
  (values))


;;;
;;; API
;;;

(define (fp/get-ps-fname-from-pdf pdfname)
  (and pdfname
       (format #f "~A/~A.ps" (dirname pdfname) (basename pdfname ".pdf"))))

(define (fp/common-filenames reference pdfname)
  (let* ((p-dir (ref %foliot-store 'printdir))
	 ;; (reference (gensym))
	 (uname (sys/get 'uname))

	 (short-filename-lvars   (format #f "~A-~A-local-variables" uname reference))
	 (tex-filename-lvars     (format #f "~A.tex" short-filename-lvars))
	 (filename-lvars         (format #f "~A/~A"
					 p-dir
					 tex-filename-lvars))
	 (lvars-tex-file         (make <tex-file>
				   #:p-directory    p-dir ;; class allocated
				   #:short-filename short-filename-lvars
				   #:tex-filename   tex-filename-lvars
				   #:full-filename  filename-lvars))

	 (short-filename-draft (format #f "~A-~A-draft" uname reference))
	 (ps-filename-draft    (format #f "~A.ps" short-filename-draft))
	 (pdf-filename-draft   (format #f "~A.pdf" short-filename-draft))
	 (tex-filename-draft   (format #f "~A.tex" short-filename-draft))
	 (filename-draft       (format #f "~A/~A" 
					   p-dir
					   tex-filename-draft))
	 (draft-tex-file       (make <tex-file>
				     #:short-filename short-filename-draft
				     #:tex-filename   tex-filename-draft
				     #:ps-filename    ps-filename-draft
				     #:pdf-filename   pdf-filename-draft
				     #:full-filename  filename-draft))
	 
	 (short-filename-draftLT (format #f "~A-~A-draftLT" uname reference))
	 (ps-filename-draftLT    (format #f "~A.ps" short-filename-draftLT))
	 (pdf-filename-draftLT   (format #f "~A.pdf" short-filename-draftLT))
	 (tex-filename-draftLT   (format #f "~A.tex" short-filename-draftLT))
	 (filename-draftLT       (format #f "~A/~A" 
					   p-dir
					   tex-filename-draftLT))
	 (draftLT-tex-file       (make <tex-file>
				     #:short-filename short-filename-draftLT
				     #:tex-filename   tex-filename-draftLT
				     #:ps-filename    ps-filename-draftLT
				     #:pdf-filename   pdf-filename-draftLT
				     #:full-filename  filename-draftLT))

	 (short-filename-commercial (format #f "~A-~A-commercial" uname reference))
	 (ps-filename-commercial    (format #f "~A.ps" short-filename-commercial))
	 (pdf-filename-commercial   (format #f "~A.pdf" short-filename-commercial))
	 (tex-filename-commercial   (format #f "~A.tex" short-filename-commercial))
	 (filename-commercial       (format #f "~A/~A" 
					   p-dir
					   tex-filename-commercial))
	 (commercial-tex-file       (make <tex-file>
				     #:short-filename short-filename-commercial
				     #:tex-filename   tex-filename-commercial
				     #:ps-filename    ps-filename-commercial
				     #:pdf-filename   pdf-filename-commercial
				     #:full-filename  filename-commercial))

	 (tex-files     (make <tex-files>
			  #:pdf        pdfname
			  #:ps         (fp/get-ps-fname-from-pdf pdfname)
			  #:doc-ref    reference
			  #:lvars      lvars-tex-file
			  #:draft      draft-tex-file
			  #:draftLT      draftLT-tex-file
			  #:commercial commercial-tex-file)))
    ;; (show-me tex-files)
    tex-files))


;;;
;;; Real printing/pdf related stuff
;;;

(define (fp/compile-tex-file tex-file-object)
  (let* ((dir (p-directory tex-file-object))
	 (fname (short-filename tex-file-object))
	 (paper-size "a4") ;; we'll get there
	 (compile-command (format #f "cd ~A; pdflatex ~A > /dev/null; pdflatex ~A > /dev/null;"
				  dir fname fname)))
    ;; (format #t "~S~%" compile-command)
    (system compile-command)))

(define (fp/write-pdf tex-files mode)
  (let* ((main-tex-file-instance (case mode
				   ((draft) (draft tex-files))
				   ((commercial) (commercial tex-files))))
	 (p-dir (p-directory main-tex-file-instance))
	 (pdf-temp-name (pdf-filename main-tex-file-instance))
	 (shortname (short-filename main-tex-file-instance))
	 (pdf-fullname (pdf tex-files))
	 #;(pdf-command (format #f "cd ~A; ps2pdf ~A.ps ~A.pdf" p-dir shortname shortname))
	 (complete-command (format #f "cd ~A; cp -f ~A ~A"
				   p-dir #;pdf-command
				   pdf-temp-name pdf-fullname)))
    ;; (format #t "~S~%" complete-command)
    (system complete-command)))

(define (fp/print-ps-file tex-files acc printer)
  (let* ((tex-file-object (acc tex-files))
	 (temp-dir        (p-directory tex-file-object))
	 (ps-fname        (ps-filename tex-file-object))
	 (print-command   (format #f "cd ~A; lp -d ~A ~A"
				  temp-dir
				  printer
				  ps-fname)))
    ;; (format #t "~S~%" print-command)
    (system print-command)))

(define (fp/write-printer tex-files mode printer)
  (case mode
    ((draft)
     ;; here below, draft is the accessor, not the mode :)
     (fp/print-ps-file tex-files draft printer)) 
    ((commercial)
     ;; dito, but commercial instead of draft
     (fp/print-ps-file tex-files commercial printer))))


#!

(define tf
  (fp/common-filenames "foliot-2011.10.31-draft"
		       (string-append (sys/get 'udir) "/foliot/foliot-2011.10.31-draft.pdf")))
(show-me tf)

!#

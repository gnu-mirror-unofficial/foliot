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


(define-module (foliot globals)
  #:use-module (oop goops)
  #:use-module (grip module)
  #:use-module (grip store)
  #:use-module (grip xft)

  #:export (%foliot-store
            %xft-store))


(eval-when (expand load eval)
  (re-export-public-interface (oop goops)
                              (grip store)))


(define %foliot-store #f)
(define %xft-store #f)

(eval-when (expand load eval)
  (set! %foliot-store
        (let* ((store (make <store>))
               (foliotdir (dirname (search-path %load-path "foliot/foliot.scm")))
               (pofdir (string-append foliotdir "/pof"))
               (gtkrcdir (string-append foliotdir "/gtkrc"))
               (iconsdir (string-append foliotdir "/icons"))
               (gladedir (string-append foliotdir "/glade"))
               (latexdir (string-append foliotdir "/latex"))
               (printdir "/tmp"))
          (init! store
                 `((foliotdir . ,foliotdir)
                   (pofdir . ,pofdir)
                   (iconsdir . ,iconsdir)
                   (gladedir . ,gladedir)
                   (gladefile . ,(string-append gladedir "/foliot.glade"))
                   (gtkrcdir . ,gtkrcdir)
                   (gtkrcfile . ,(string-append gtkrcdir "/gtkrc.foliot"))
                   (latexdir . ,latexdir)
                   (printdir . ,printdir)
                   (imported-ids-delta . 1000000))
                 #:no-checks #t)))
  (set! %xft-store (xft-store)))

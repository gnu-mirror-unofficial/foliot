;; -*- mode: scheme; coding: utf-8 -*-

;;;;
;;;; Copyright (C) 2018
;;;; Free Software Foundation, Inc.

;;;; This file is part of GNU Foliot.

;;;; GNU Foliot is free software; you can redistribute it and/or modify
;;;; it under the terms of the GNU General Public License as published
;;;; by the Free Software Foundation; either version 3 of the License,
;;;; or (at your option) any later version.

;;;; GNU Foliot is distributed in the hope that it will be useful, but
;;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;;; General Public License for more details.

;;;; You should have received a copy of the GNU General Public License
;;;; along with GNU Foliot.  If not, see
;;;; <https://www.gnu.org/licenses/gpl.html>.
;;;;

;;; Commentary:

;;; Code:


(define-module (tests globals)
  #:use-module (oop goops)
  #:use-module (unit-test)
  #:use-module (foliot globals))


(define-class <foliot-tests-globals> (<test-case>))


(define-method (test-foliot-store (self <foliot-tests-globals>))
  (let ((store %foliot-store))
    (assert-true (get store 'foliotdir))
    (assert-true (get store 'pofdir))
    (assert-true (get store 'gladedir))
    (assert-true (get store 'iconsdir))
    (assert-true (get store 'gladefile))
    (assert-true (get store 'gtkrcfile))
    (assert-true (get store 'latexdir))
    (assert-true (get store 'printdir))
    (assert-true (get store 'imported-ids-delta))))


(exit-with-summary (run-all-defined-test-cases))

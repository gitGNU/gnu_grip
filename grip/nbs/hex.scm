;; -*- mode: scheme; coding: utf-8 -*-

;;;;
;;;; Copyright (C) 2011 - 2015
;;;; David Pirotte <david at altosw dot be>

;;;; This file is part of Grip.
;;;; A Grip of Really Important Procedures.

;;;; Grip is free software: you can redistribute it and/or modify it
;;;; under the terms of the GNU General Public License as published by
;;;; the Free Software Foundation, either version 3 of the License, or
;;;; (at your option) any later version.

;;;; Grip is distributed in the hope that it will be useful WARRANTY;
;;;; without even the implied warranty of MERCHANTABILITY or FITNESS FOR
;;;; A PARTICULAR PURPOSE.  See the GNU General Public License for more
;;;; details.

;;;; You should have received a copy of the GNU General Public License
;;;; along with Grip.  If not, see <http://www.gnu.org/licenses/>.
;;;;

;;; Commentary:

;; This code needs review and/or be rewritten: it is imported almost 'as
;; is' from 'common', grip's ancestor, started while using guile-1.6, a
;; past where I could not trust the module system, goops, and myself [:)
;; as a schemer].  But I want grip-0.1.0 and kise-0.9.5 ready asap to
;; talk to svannah, gitorious has been murdered.

;;; Code:


(define-module (grip nbs hex)
  #:use-module (ice-9 format)
  #:use-module (ice-9 receive)
  #:use-module (grip nbs fp)

  #:export (hex/colour2int
	    hex/colour2dec))


(define (wifs col-str)
  (with-input-from-string (format #f "#x~A" col-str) read))

(define (cei int)
  (fp/round (exact->inexact (/ int 255)) 2))

(define (hex/colour2int hex)
  (let* ((hex-str   (if (string? hex) hex (symbol->string hex)))
	 (str-red   (substring hex-str 0 2))
	 (str-green (substring hex-str 2 4))
	 (str-blue  (substring hex-str 4)))
    (values (wifs str-red)
	    (wifs str-green)
	    (wifs str-blue))))

(define (hex/colour2dec hex)
  (receive (red green blue)
      (hex/colour2int hex)
    (values (cei red)
	    (cei green)
	    (cei blue))))


#!

(hex/colour2int '839fa0)
(hex/colour2dec '839fa0)

(hex/colour2dec 'ad7fa8)

!#


;; -*- mode: scheme; coding: utf-8 -*-

;;;;
;;;; Copyright (C) 2011 - 2016
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


(define-module (grip nbs str2nb)
  #:use-module (ice-9 format)
  #:use-module (grip reexport)
  #:use-module (grip strings)
  #:use-module (grip nbs fp)
  
  #:export (string-to-int-rfp-or-string
	    csv-string-to-numbers))


(eval-when (expand load eval)
  (re-export-public-interface (grip strings)
			      (grip nbs fp)))


(define* (string-to-int-rfp-or-string token #:key (dec 2))
  (let ((nb (string->number token)))
    (if nb
	(if (integer? nb) nb (fp/round nb dec))
	token)))

(define* (csv-string-to-numbers line #:key (sep #\,) (func #f))
  (map (or func string->number) (str/get-tokens line (char-set sep))))


#!

(csv-string-to-numbers "19,7812,94.903,243.442,512.250,0.374,1,.917")
(csv-string-to-numbers "Blue	7812	243.442	512.250	0.374	1	.917"
		       #:sep #\Tab
		       #:func string-to-int-rfp-or-string)

!#


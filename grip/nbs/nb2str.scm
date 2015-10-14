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


(define-module (grip nbs nb2str)
  #:use-module (ice-9 format)
  #:use-module (grip nbs fp)
  
  #:export (nb/commify-1
	    nb/commify
	    nb/commify-from-str
	    nb/decommify
	    nb/comfy-1d
	    nb/numprintify))


(define (nb/commify-1 num dec . padding)
  (let* ((neg (negative? num))
	 (str (fp/round num dec))
	 (dot (string-index str #\.))
	 (int (string->number (substring str 0 dot)))
	 (dec (substring str (1+ dot))))
    (case dec
      ((0)
       (if (and neg
		(= int 0)) ;; int part is already signed when not zero
	   (if (null? padding)
	       (format #f "-~,,'.:D" int)
	       (format #f "-~,,'.:D~A" int (car padding)))
	   (if (null? padding)
	       (format #f "~,,'.:D" int)
	       (format #f "~,,'.:D~A" int (car padding)))))
      (else
       (if (and neg
		(= int 0)) ;; int part is already signed when not zero
	   (if (null? padding)
	       (string-append (format #f "-~,,'.:D" int) "," dec)
	       (string-append (format #f "-~,,'.:D" int) "," dec (car padding)))
	   (if (null? padding)
	       (string-append (format #f "~,,'.:D" int) "," dec)
	       (string-append (format #f "~,,'.:D" int) "," dec (car padding))))))))

(define (nb/commify num . dec)
  (if (null? dec)
      (nb/commify-1 num 2)
      (nb/commify-1 num (car dec))))

(define (nb/comfy-1d num)
  (nb/commify-1 num 1))

(define (nb/commify-from-str num-str)
  (format #f "~,,'.:D" (string->number num-str)))

(define (nb/decommify str-num)
  ;; 1- removing the .
  ;; 2- replacing , by .
  (let* ((no-dots (string-delete #\. str-num))
	 (com-pos (string-index no-dots #\,)))
    (if com-pos
	(string-set! no-dots com-pos #\.))
    no-dots))

(define (nb/numprintify num . dec)
  ;; used to 'send' a float to latex [numprint]
  (if (null? dec)
      (format #f "~,2F" num)
      (case (car dec)
	((0) 
	 (inexact->exact (round num)))
	(else
	 (fp/round num (car dec))))))


#!

(nb/commify 12)
(nb/commify 12 1)
(nb/commify 1243567 1)
(nb/commify 1243567.56789)
(nb/commify 1243567.56789 1)

(nb/decommify "2.234,34")
(nb/decommify "3.122.234,34")
(nb/decommify "-3.122.234,34")
(nb/decommify "-434")

!#

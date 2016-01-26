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


(define-module (grip nbs fp)
  #:use-module (ice-9 format)

  #:export (%fp/pi
	    %fp/default-precision
	    fp/zero?
	    fp/=?
	    fp/<?
	    fp/>?
	    ;; float<=?
	    ;; float>?
	    ;; float>=?
	    fp/round
	    fp/avg))


(define %fp/pi
  3.141592654)

;; (defun short-float/= (f1 f2 &optional (precision *precision*))
;;   (declare (inline short-float/=)
;; 	   (short-float f1 f2 precision)
;; 	   (optimize (safety 0) (space 0) (speed 3) (debug 0)))
;;   (> (abs (- f1 f2)) precision))

;; (defun short-float< (f1 &optional (precision *precision*))
;;   (declare (inline short-float/=)
;; 	   (short-float f1 f2 precision)
;; 	   (optimize (safety 0) (space 0) (speed 3) (debug 0)))
;;   (< (abs f1) precision))

(define %fp/default-precision 1.0E-4)

(define (fp/zero? a-float . prec)
  (if (null? prec)
      (<= (abs a-float) %fp/default-precision)
      (<= (abs a-float) (car prec))))

(define (fp/=? f1 f2 . prec)
  (if (null? prec)
      (<= (abs (- f1 f2)) %fp/default-precision)
      (<= (abs (- f1 f2)) (car prec))))

(define (fp/<? f1 f2 . prec)
  (let ((diff (- f1 f2)))
    (if (null? prec)
	(and (negative? diff)
	     (> (abs diff) %fp/default-precision))
	(and (negative? diff)
	     (> (abs diff) (car prec))))))

(define (fp/>? f1 f2 . prec)
  (let ((diff (- f1 f2)))
    (if (null? prec)
	(and (positive? diff)
	     (> (abs diff) %fp/default-precision))
	(and (positive? diff)
	     (> (abs diff) (car prec))))))

(define (fp/round float . dec)
  (let ((m (if (pair? dec)
	       (expt 10 (car dec))
	       100)))
    (/ (round (* m float)) m)))

(define (fp/avg vals . dec)
  ;; named fp/avg for consistency with the rest of this module, but vals
  ;; can be composed of any [non complex] numbers.
  (let ((val (/ (apply + vals) (length vals))))
    (if (pair? dec)
	(fp/round val (car dec))
	val)))


#!

(fp/zero? 0.001)
(fp/zero? 0.0001)

(fp/=? 0.001 0.0001)
(fp/=? 0.0001 0.0001)
(fp/=? 0.0001 -0.0001)

(define f1 0.0001)
(define f2 0.001)
(fp/<? f1 f2)

(fp/round 13.3456723 3)

!#


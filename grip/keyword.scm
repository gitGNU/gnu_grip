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

;;; Code:


(define-module (grip keyword)
  #:use-module (grip push)
  
  #:export (split-keyword-args))


(define (split-keyword-args-1 args grab a b)
  (if (null? args)
      (values a b)
      (if (memq (car args) grab)
	  (split-keyword-args-1 (cddr args)
				grab
				a
				(push* (car args) (cadr args) b))
	  (split-keyword-args-1 (cddr args)
				grab
				(push* (car args) (cadr args) a)
				b))))

(define (split-keyword-args args grab-these)
  (split-keyword-args-1 args grab-these (list) (list)))


#!

;; missing good example/mini tests

!#

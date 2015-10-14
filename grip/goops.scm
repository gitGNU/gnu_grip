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

;;; Code:


(define-module (grip goops)
  #:use-module (srfi srfi-1)
  #:use-module (oop goops)
  #:use-module (grip reexport)

  #:export (class-direct-virtual-slots
	    class-virtual-slots
	    describe))


(eval-when (expand load eval)
  (re-export-public-interface (oop goops)))


(define-method (class-direct-virtual-slots (c <class>))
  (filter-map (lambda (slot-definition)
		(and (eq? (slot-definition-allocation slot-definition)
			  #:virtual)
		     slot-definition))
      (class-direct-slots c)))

(define-method (class-virtual-slots (c <class>))
  (filter-map (lambda (slot-definition)
		(and (eq? (slot-definition-allocation slot-definition)
			  #:virtual)
		     slot-definition))
      (class-slots c)))

(define-method (describe (self <object>))
  (format #t "~S - instance of ~A~%"
	  self
	  (class-name (class-of self)))
  (format #t "  slots and values are:~%")
  (for-each (lambda (slot)
	      (let ((name (slot-definition-name slot)))
		(format #t "    ~S = ~A~%"
			name
			(if (slot-bound? self name) 
			    (format #f "~S" (slot-ref self name))
			    "#<unbound>"))))
	    (class-slots (class-of self)))
  *unspecified*)
